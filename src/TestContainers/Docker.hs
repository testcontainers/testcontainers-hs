{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module TestContainers.Docker
  ( MonadDocker,

    -- * Configuration
    Config (..),
    defaultDockerConfig,
    determineConfig,

    -- * Exeuction tracing
    Tracer,
    Trace (..),
    newTracer,
    withTrace,

    -- * Docker image
    ImageTag,
    Image,
    imageTag,

    -- * Docker container
    ContainerId,
    Container,
    containerId,
    containerImage,
    containerAlias,
    containerGateway,
    containerIp,
    containerPort,
    containerAddress,
    containerReleaseKey,

    -- * Referring to images
    ToImage,
    fromTag,
    fromBuildContext,
    fromDockerfile,
    build,

    -- * Exceptions
    DockerException (..),

    -- * Running containers
    ContainerRequest,
    containerRequest,
    setName,
    setFixedName,
    setSuffixedName,
    setRandomName,
    setCmd,
    setVolumeMounts,
    setRm,
    setEnv,
    setNetwork,
    setLink,
    setExpose,
    setWaitingFor,
    run,

    -- * Managing the container lifecycle
    InspectOutput,
    inspect,
    stop,
    kill,
    rm,
    withLogs,

    -- * Wait for containers to become ready
    WaitUntilReady,
    waitUntilReady,

    -- * Only block for defined amounts of time
    TimeoutException (..),
    waitUntilTimeout,

    -- * Wait until a specific pattern appears in the logs
    waitWithLogs,
    UnexpectedEndOfPipe (..),
    Pipe (..),
    waitForLogLine,

    -- * Misc. Docker functions
    dockerHostOs,
    isDockerOnLinux,

    -- * Wait until a socket is reachable
    waitUntilMappedPortReachable,

    -- * Wait until the http server responds with a specific status code
    waitForHttp,

    -- * Reexports for convenience
    ResIO,
    runResourceT,
    (&),
  )
where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Exception (IOException, throw)
import Control.Monad (replicateM, unless)
import Control.Monad.Catch
  ( Exception,
    MonadCatch,
    MonadMask,
    MonadThrow,
    bracket,
    throwM,
    try,
  )
import Control.Monad.Fix (mfix)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.IO.Unlift (MonadUnliftIO (withRunInIO))
import Control.Monad.Reader (MonadReader (..), runReaderT)
import Control.Monad.Trans.Resource
  ( MonadResource (liftResourceT),
    ReleaseKey,
    ResIO,
    register,
    runResourceT,
  )
import Data.Aeson (Value, decode')
import qualified Data.Aeson.Optics as Optics
import qualified Data.ByteString.Lazy.Char8 as LazyByteString
import qualified Data.ByteString.UTF8 as BSU
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.List (find)
import Data.String (IsString (..))
import Data.Text (Text, pack, strip, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText
import GHC.Stack (withFrozenCallStack)
import Network.HTTP.Client
  ( HttpException,
    Request (..),
    defaultManagerSettings,
    defaultRequest,
    httpNoBody,
    newManager,
    responseStatus,
  )
import Network.HTTP.Types (statusCode)
import qualified Network.Socket as Socket
import Optics.Fold (pre)
import Optics.Operators ((^?))
import Optics.Optic ((%))
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import System.IO (Handle, hClose)
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Process as Process
import qualified System.Random as Random
import System.Timeout (timeout)
import TestContainers.Trace (Trace (..), Tracer, newTracer, withTrace)
import Prelude hiding (error, id)
import qualified Prelude

-- | Configuration for defaulting behavior.
--
-- @since 0.2.0.0
data Config = Config
  { -- | The number of seconds to maximally wait for a container to
    -- become ready. Default is `Just 60`.
    --
    -- @Nothing@ <=> waits indefinitely.
    configDefaultWaitTimeout :: Maybe Int,
    -- | Traces execution inside testcontainers library.
    configTracer :: Tracer
  }

-- | Default configuration.
--
-- @since 0.2.0.0
defaultDockerConfig :: Config
defaultDockerConfig =
  Config
    { configDefaultWaitTimeout = Just 60,
      configTracer = mempty
    }

-- | Autoselect the default configuration depending on wether you use Docker For
-- Mac or not.
determineConfig :: IO Config
determineConfig =
  pure defaultDockerConfig

-- | Failing to interact with Docker results in this exception
-- being thrown.
--
-- @since 0.1.0.0
data DockerException
  = DockerException
      { -- | Exit code of the underlying Docker process.
        exitCode :: ExitCode,
        -- | Arguments that were passed to Docker.
        args :: [Text],
        -- | Docker's STDERR output.
        stderr :: Text
      }
  | InspectUnknownContainerId {id :: ContainerId}
  | InspectOutputInvalidJSON {id :: ContainerId}
  | InspectOutputMissingNetwork {id :: ContainerId}
  | InspectOutputUnexpected {id :: ContainerId}
  | UnknownPortMapping
      { -- | Id of the `Container` that we tried to lookup the
        -- port mapping.
        id :: ContainerId,
        -- | Textual representation of port mapping we were
        -- trying to look up.
        port :: Text
      }
  deriving (Eq, Show)

instance Exception DockerException

-- | Docker related functionality is parameterized over this `Monad`.
--
-- @since 0.1.0.0
type MonadDocker m =
  (MonadIO m, MonadMask m, MonadThrow m, MonadCatch m, MonadResource m, MonadReader Config m)

-- | Parameters for a running a Docker container.
--
-- @since 0.1.0.0
data ContainerRequest = ContainerRequest
  { toImage :: ToImage,
    cmd :: Maybe [Text],
    env :: [(Text, Text)],
    exposedPorts :: [Int],
    volumeMounts :: [(Text, Text)],
    network :: Maybe Text,
    links :: [ContainerId],
    naming :: NamingStrategy,
    rmOnExit :: Bool,
    readiness :: Maybe WaitUntilReady
  }

-- | Parameters for a naming a Docker container.
--
-- @since 0.4.0.0
data NamingStrategy
  = RandomName
  | FixedName Text
  | SuffixedName Text

-- | Default `ContainerRequest`. Used as base for every Docker container.
--
-- @since 0.1.0.0
containerRequest :: ToImage -> ContainerRequest
containerRequest image =
  ContainerRequest
    { toImage = image,
      naming = RandomName,
      cmd = Nothing,
      env = [],
      exposedPorts = [],
      volumeMounts = [],
      network = Nothing,
      links = [],
      rmOnExit = True,
      readiness = Nothing
    }

-- | Set the name of a Docker container. This is equivalent to invoking @docker run@
-- with the @--name@ parameter.
--
-- @since 0.1.0.0
setName :: Text -> ContainerRequest -> ContainerRequest
setName = setFixedName
{-# DEPRECATED setName "See setFixedName" #-}

-- | Set the name of a Docker container. This is equivalent to invoking @docker run@
-- with the @--name@ parameter.
--
-- @since 0.4.0.0
setFixedName :: Text -> ContainerRequest -> ContainerRequest
setFixedName newName req =
  -- TODO error on empty Text
  req {naming = FixedName newName}

-- | Set the name randomly given of a Docker container. This is equivalent to omitting
--  the @--name@ parameter calling @docker run@.
--
-- @since 0.4.0.0
setRandomName :: ContainerRequest -> ContainerRequest
setRandomName req =
  -- TODO error on empty Text
  req {naming = RandomName}

-- | Set the name randomly suffixed of a Docker container. This is equivalent to invoking
-- @docker run@ with the @--name@ parameter.
--
-- @since 0.4.0.0
setSuffixedName :: Text -> ContainerRequest -> ContainerRequest
setSuffixedName preffix req =
  -- TODO error on empty Text
  req {naming = SuffixedName preffix}

-- | The command to execute inside the Docker container. This is the equivalent
-- of passing the command on the @docker run@ invocation.
--
-- @since 0.1.0.0
setCmd :: [Text] -> ContainerRequest -> ContainerRequest
setCmd newCmd req =
  req {cmd = Just newCmd}

-- | The volume mounts to link to Docker container. This is the equivalent
-- of passing the command on the @docker run -v@ invocation.
setVolumeMounts :: [(Text, Text)] -> ContainerRequest -> ContainerRequest
setVolumeMounts newVolumeMounts req =
  req {volumeMounts = newVolumeMounts}

-- | Wether to remove the container once exited. This is equivalent to passing
-- @--rm@ to @docker run@. (default is `True`).
--
-- @since 0.1.0.0
setRm :: Bool -> ContainerRequest -> ContainerRequest
setRm newRm req =
  req {rmOnExit = newRm}

-- | Set the environment for the container. This is equivalent to passing @--env key=value@
-- to @docker run@.
--
-- @since 0.1.0.0
setEnv :: [(Text, Text)] -> ContainerRequest -> ContainerRequest
setEnv newEnv req =
  req {env = newEnv}

-- | Set the network the container will connect to. This is equivalent to passing
-- @--network network_name@ to @docker run@.
--
-- @since 0.4.0.0
setNetwork :: Text -> ContainerRequest -> ContainerRequest
setNetwork networkName req =
  req {network = Just networkName}

-- | Set link on the container. This is equivalent to passing @--link other_container@
-- to @docker run@.
--
-- @since 0.1.0.0
setLink :: [ContainerId] -> ContainerRequest -> ContainerRequest
setLink newLink req =
  req {links = newLink}

-- | Set exposed ports on the container. This is equivalent to setting @--publish $PORT@ to
-- @docker run@. Docker assigns a random port for the host port. You will have to use `containerIp`
-- and `containerPort` to connect to the published port.
--
-- @
--   container <- `run` $ `containerRequest` `redis` & `setExpose` [ 6379 ]
--   let (redisHost, redisPort) = (`containerIp` container, `containerPort` container 6379)
--   print (redisHost, redisPort)
-- @
--
-- @since 0.1.0.0
setExpose :: [Int] -> ContainerRequest -> ContainerRequest
setExpose newExpose req =
  req {exposedPorts = newExpose}

-- | Set the waiting strategy on the container. Depending on a Docker container
-- it can take some time until the provided service is ready. You will want to
-- use to `setWaitingFor` to block until the container is ready to use.
--
-- @since 0.1.0.0
setWaitingFor :: WaitUntilReady -> ContainerRequest -> ContainerRequest
setWaitingFor newWaitingFor req =
  req {readiness = Just newWaitingFor}

-- | Runs a Docker container from an `Image` and `ContainerRequest`. A finalizer
-- is registered so that the container is aways stopped when it goes out of scope.
-- This function is essentially @docker run@.
--
-- @since 0.1.0.0
run :: (MonadDocker m) => ContainerRequest -> m Container
run request = do
  let ContainerRequest
        { toImage,
          naming,
          cmd,
          env,
          exposedPorts,
          volumeMounts,
          network,
          links,
          rmOnExit,
          readiness
        } = request

  image@Image {tag} <- runToImage toImage

  name <-
    case naming of
      RandomName -> return Nothing
      FixedName n -> return $ Just n
      SuffixedName prefix ->
        Just . (prefix <>) . ("-" <>) . pack
          <$> replicateM 6 (Random.randomRIO ('a', 'z'))

  let dockerRun :: [Text]
      dockerRun =
        concat $
          [["run"]]
            ++ [["--detach"]]
            ++ [["--name", containerName] | Just containerName <- [name]]
            ++ [["--env", variable <> "=" <> value] | (variable, value) <- env]
            ++ [["--publish", pack (show port)] | port <- exposedPorts]
            ++ [["--network", networkName] | Just networkName <- [network]]
            ++ [["--link", container] | container <- links]
            ++ [["--volume", src <> ":" <> dest] | (src, dest) <- volumeMounts]
            ++ [["--rm"] | rmOnExit]
            ++ [[tag]]
            ++ [command | Just command <- [cmd]]

  config@Config {configTracer} <- ask

  stdout <- docker configTracer dockerRun

  let id :: ContainerId
      !id =
        -- N.B. Force to not leak STDOUT String
        strip (pack stdout)

      -- Careful, this is really meant to be lazy
      ~inspectOutput =
        unsafePerformIO $
          internalInspect configTracer id

  container <- liftResourceT $ mfix $ \container -> do
    -- Note: We have to tie the knot as the resulting container
    -- carries the release key as well.
    releaseKey <- register $ runReaderT (runResourceT (stop container)) config
    pure $
      Container
        { id,
          releaseKey,
          image,
          inspectOutput,
          config
        }

  case readiness of
    Just wait ->
      waitUntilReady container wait
    Nothing ->
      pure ()

  pure container

-- | Internal function that runs Docker. Takes care of throwing an exception
-- in case of failure.
--
-- @since 0.1.0.0
docker :: (MonadIO m) => Tracer -> [Text] -> m String
docker tracer args =
  dockerWithStdin tracer args ""

-- | Internal function that runs Docker. Takes care of throwing an exception
-- in case of failure.
--
-- @since 0.1.0.0
dockerWithStdin :: (MonadIO m) => Tracer -> [Text] -> Text -> m String
dockerWithStdin tracer args stdin = liftIO $ do
  (exitCode, stdout, stderr) <-
    Process.readProcessWithExitCode
      "docker"
      (map unpack args)
      (unpack stdin)

  withTrace tracer (TraceDockerInvocation args stdin exitCode)

  -- TODO output these concurrently with the process
  traverse_ (withTrace tracer . TraceDockerStdout . pack) (lines stdout)
  traverse_ (withTrace tracer . TraceDockerStderr . pack) (lines stderr)

  case exitCode of
    ExitSuccess -> pure stdout
    _ ->
      throwM $
        DockerException
          { exitCode,
            args,
            stderr = pack stderr
          }

-- | Kills a Docker container. `kill` is essentially @docker kill@.
--
-- @since 0.1.0.0
kill :: (MonadDocker m) => Container -> m ()
kill Container {id} = do
  tracer <- askTracer
  _ <- docker tracer ["kill", id]
  return ()

-- | Stops a Docker container. `stop` is essentially @docker stop@.
--
-- @since 0.1.0.0
stop :: (MonadDocker m) => Container -> m ()
stop Container {id} = do
  tracer <- askTracer
  _ <- docker tracer ["stop", id]
  return ()

-- | Remove a Docker container. `rm` is essentially @docker rm -f@
--
-- @since 0.1.0.0
rm :: (MonadDocker m) => Container -> m ()
rm Container {id} = do
  tracer <- askTracer
  _ <- docker tracer ["rm", "-f", "-v", id]
  return ()

-- | Access STDOUT and STDERR of a running Docker container. This is essentially
-- @docker logs@ under the hood.
--
-- @since 0.1.0.0
withLogs :: forall m a. (MonadDocker m) => Container -> (Handle -> Handle -> m a) -> m a
withLogs Container {id} logger = do
  let acquire :: m (Handle, Handle, Handle, Process.ProcessHandle)
      acquire =
        liftIO $
          Process.runInteractiveProcess
            "docker"
            ["logs", "--follow", unpack id]
            Nothing
            Nothing

      release :: (Handle, Handle, Handle, Process.ProcessHandle) -> m ()
      release (stdin, stdout, stderr, handle) =
        liftIO $
          Process.cleanupProcess
            (Just stdin, Just stdout, Just stderr, handle)

  bracket acquire release $ \(stdin, stdout, stderr, _handle) -> do
    -- No need to keep it around...
    liftIO $ hClose stdin
    logger stdout stderr

-- | A tag to a Docker image.
--
-- @since 0.1.0.0
type ImageTag = Text

-- | A description of how to build an `Image`.
--
-- @since 0.1.0.0
data ToImage = ToImage
  { runToImage :: forall m. (MonadDocker m) => m Image,
    applyToContainerRequest :: ContainerRequest -> ContainerRequest
  }

-- | Build the `Image` referred to by the argument. If the construction of the
-- image is expensive (e.g. a call to `fromBuildContext`) we don't want to
-- repeatedly build the image. Instead, `build` can be used to execute the
-- underlying Docker build once and re-use the resulting `Image`.
--
-- @since 0.1.0.0
build :: (MonadDocker m) => ToImage -> m ToImage
build toImage@ToImage {applyToContainerRequest} = do
  image <- runToImage toImage
  return $
    ToImage
      { runToImage = pure image,
        applyToContainerRequest
      }

-- | Default `ToImage`. Doesn't apply anything to to `ContainerRequests`.
--
-- @since 0.1.0.0
defaultToImage :: (forall m. (MonadDocker m) => m Image) -> ToImage
defaultToImage action =
  ToImage
    { runToImage = action,
      applyToContainerRequest = \x -> x
    }

-- | Get an `Image` from a tag.
--
-- @since 0.1.0.0
fromTag :: ImageTag -> ToImage
fromTag tag = defaultToImage $ do
  tracer <- askTracer
  output <- docker tracer ["pull", "--quiet", tag]
  return $
    Image
      { tag = strip (pack output)
      }

-- | Build the image from a build path and an optional path to the
-- Dockerfile (default is Dockerfile)
--
-- @since 0.1.0.0
fromBuildContext ::
  FilePath ->
  Maybe FilePath ->
  ToImage
fromBuildContext path mdockerfile = defaultToImage $ do
  let args
        | Just dockerfile <- mdockerfile =
            ["build", "--quiet", "-f", pack dockerfile, pack path]
        | otherwise =
            ["build", "--quiet", pack path]
  tracer <- askTracer
  output <- docker tracer args
  return $
    Image
      { tag = strip (pack output)
      }

-- | Build a contextless image only from a Dockerfile passed as `Text`.
--
-- @since 0.1.0.0
fromDockerfile ::
  Text ->
  ToImage
fromDockerfile dockerfile = defaultToImage $ do
  tracer <- askTracer
  output <- dockerWithStdin tracer ["build", "--quiet", "-"] dockerfile
  return $
    Image
      { tag = strip (pack output)
      }

-- | Identifies a container within the Docker runtime. Assigned by @docker run@.
--
-- @since 0.1.0.0
type ContainerId = Text

-- | A strategy that describes how to asses readiness of a `Container`. Allows
-- Users to plug in their definition of readiness.
--
-- @since 0.1.0.0
newtype WaitUntilReady = WaitUntilReady
  { checkContainerReady :: Config -> Container -> (Maybe Int, ResIO ())
  }

-- | The exception thrown by `waitForLine` in case the expected log line
-- wasn't found.
--
-- @since 0.1.0.0
newtype UnexpectedEndOfPipe = UnexpectedEndOfPipe
  { -- | The id of the underlying container.
    id :: ContainerId
  }
  deriving (Eq, Show)

instance Exception UnexpectedEndOfPipe

-- | The exception thrown by `waitUntilTimeout`.
--
-- @since 0.1.0.0
newtype TimeoutException = TimeoutException
  { -- | The id of the underlying container that was not ready in time.
    id :: ContainerId
  }
  deriving (Eq, Show)

instance Exception TimeoutException

-- | @waitUntilTimeout n waitUntilReady@ waits @n@ seconds for the container
-- to be ready. If the container is not ready by then a `TimeoutException` will
-- be thrown.
--
-- @since 0.1.0.0
waitUntilTimeout :: Int -> WaitUntilReady -> WaitUntilReady
waitUntilTimeout seconds wait = WaitUntilReady $ \config container ->
  case checkContainerReady wait config container of
    (Nothing, check) ->
      (Just seconds, check)
    (Just innerTimeout, check)
      | innerTimeout > seconds ->
          (Just seconds, check)
      | otherwise ->
          (Just innerTimeout, check)

-- | Waits for a specific http status code.
-- This combinator should always be used with `waitUntilTimeout`.
--
-- @since 0.4.0.0
waitForHttp ::
  Int ->
  String ->
  [Int] ->
  WaitUntilReady
waitForHttp port path acceptableStatusCodes = WaitUntilReady $ \config container ->
  let Config {configTracer} = config

      wait = newManager defaultManagerSettings >>= retry
      retry manager = do
        (endpointHost, endpointPort) <- containerAddress container port
        let req =
              defaultRequest
                { host = encodeUtf8 endpointHost,
                  port = endpointPort,
                  path = BSU.fromString path
                }

        result <- try $ statusCode . responseStatus <$> httpNoBody req manager
        hasSucceeded <- case result of
          Right code -> do
            withTrace
              configTracer
              (TraceHttpCall endpointHost endpointPort (Right code))
            pure $ code `elem` acceptableStatusCodes
          Left (exception :: HttpException) -> do
            withTrace
              configTracer
              (TraceHttpCall endpointHost endpointPort (Left $ show exception))
            pure False
        unless hasSucceeded $ threadDelay 500000 *> retry manager
   in (Nothing, liftIO wait)

-- | Waits until the port of a container is ready to accept connections.
-- This combinator should always be used with `waitUntilTimeout`.
--
-- @since 0.1.0.0
waitUntilMappedPortReachable ::
  Int ->
  WaitUntilReady
waitUntilMappedPortReachable port = WaitUntilReady $ \config container ->
  withFrozenCallStack $
    let Config {configTracer} = config

        resolve endpointHost endpointPort = do
          let hints = Socket.defaultHints {Socket.addrSocketType = Socket.Stream}
          head <$> Socket.getAddrInfo (Just hints) (Just endpointHost) (Just (show endpointPort))

        open addr = do
          socket <-
            Socket.socket
              (Socket.addrFamily addr)
              (Socket.addrSocketType addr)
              (Socket.addrProtocol addr)
          Socket.connect
            socket
            (Socket.addrAddress addr)
          pure socket

        retry = do
          (endpointHost, endpointPort) <- containerAddress container port

          result <- try (resolve (unpack endpointHost) endpointPort >>= open)
          case result of
            Right socket -> do
              withTrace configTracer (TraceOpenSocket endpointHost endpointPort Nothing)
              Socket.close socket
              pure ()
            Left (exception :: IOException) -> do
              withTrace
                configTracer
                (TraceOpenSocket endpointHost endpointPort (Just exception))
              threadDelay 500000
              retry
     in (Nothing, liftIO retry)

-- | A low-level primitive that allows scanning the logs for specific log lines
-- that indicate readiness of a container.
--
-- The `Handle`s passed to the function argument represent @stdout@ and @stderr@
-- of the container.
--
-- @since 0.1.0.0
waitWithLogs :: (Container -> Handle -> Handle -> IO ()) -> WaitUntilReady
waitWithLogs waiter = WaitUntilReady $ \config container ->
  let check = flip runReaderT config $ withLogs container $ \stdout stderr ->
        liftIO $ waiter container stdout stderr
   in (Nothing, check)

-- | A data type indicating which pipe to scan for a specific log line.
--
-- @since 0.1.0.0
data Pipe
  = -- | Refer to logs on STDOUT.
    Stdout
  | -- | Refer to logs on STDERR.
    Stderr
  deriving (Eq, Ord, Show)

-- | Waits for a specific line to occur in the logs. Throws a `UnexpectedEndOfPipe`
-- exception in case the desired line can not be found on the logs.
--
-- Say you want to find "Ready to accept connections" in the logs on Stdout try:
--
-- @
-- waitForLogLine Stdout ("Ready to accept connections" ``LazyText.isInfixOf``)
-- @
--
-- @since 0.1.0.0
waitForLogLine :: Pipe -> (LazyText.Text -> Bool) -> WaitUntilReady
waitForLogLine whereToLook matches = waitWithLogs $ \Container {id} stdout stderr -> do
  let logs :: Handle
      logs = case whereToLook of
        Stdout -> stdout
        Stderr -> stderr

  logContent <- LazyByteString.hGetContents logs

  let logLines :: [LazyText.Text]
      logLines =
        -- FIXME: This is assuming UTF8 encoding. Do better!
        map
          (LazyText.decodeUtf8With lenientDecode)
          (LazyByteString.lines logContent)

  case find matches logLines of
    Just _ -> pure ()
    Nothing -> throwM $ UnexpectedEndOfPipe {id}

-- | Blocks until the container is ready. `waitUntilReady` might throw exceptions
-- depending on the used `WaitUntilReady` on the container.
--
-- @since 0.1.0.0
waitUntilReady :: (MonadDocker m) => Container -> WaitUntilReady -> m ()
waitUntilReady container@Container {id} waiter = do
  config@Config {configDefaultWaitTimeout, configTracer} <- ask
  let (timeoutInSeconds, check) =
        checkContainerReady waiter config container

      runAction action timeoutSeconds = do
        withTrace configTracer (TraceWaitUntilReady timeoutSeconds)
        action

      withTimeout action = case timeoutInSeconds <|> configDefaultWaitTimeout of
        Nothing ->
          runAction action Nothing
        Just seconds ->
          withRunInIO $ \runInIO -> do
            result <- timeout (seconds * 1000000) $ runInIO (runAction action (Just seconds))
            case result of
              Nothing ->
                throwM $ TimeoutException {id}
              Just _ ->
                pure ()

  liftResourceT (withTimeout check)

-- | Handle to a Docker image.
--
-- @since 0.1.0.0
data Image = Image
  { -- | The image tag assigned by Docker. Uniquely identifies an `Image`
    -- within Docker.
    tag :: ImageTag
  }
  deriving (Eq, Show)

-- | The image tag assigned by Docker. Uniquely identifies an `Image`
-- within Docker.
--
-- @since 0.1.0.0
imageTag :: Image -> ImageTag
imageTag Image {tag} = tag

-- | Handle to a Docker container.
--
-- @since 0.1.0.0
data Container = Container
  { -- | The container Id assigned by Docker, uniquely identifying this `Container`.
    id :: ContainerId,
    -- | Underlying `ReleaseKey` for the resource finalizer.
    releaseKey :: ReleaseKey,
    -- | The underlying `Image` of this container.
    image :: Image,
    -- | Configuration used to create and run this container.
    config :: Config,
    -- | Memoized output of `docker inspect`. This is being calculated lazily.
    inspectOutput :: InspectOutput
  }

-- | The parsed JSON output of docker inspect command.
--
-- @since 0.1.0.0
type InspectOutput = Value

-- | Returns the id of the container.
--
-- @since 0.1.0.0
containerId :: Container -> ContainerId
containerId Container {id} = id

-- | Returns the underlying image of the container.
--
-- @since 0.1.0.0
containerImage :: Container -> Image
containerImage Container {image} = image

-- | Returns the internal release key used for safely shutting down
-- the container. Use this with care. This function is considered
-- an internal detail.
--
-- @since 0.1.0.0
containerReleaseKey :: Container -> ReleaseKey
containerReleaseKey Container {releaseKey} = releaseKey

-- | Looks up the ip address of the container.
--
-- @since 0.1.0.0
containerIp :: Container -> Text
containerIp =
  internalContainerIp

-- | Get the IP address of a running Docker container using @docker inspect@.
internalContainerIp :: Container -> Text
internalContainerIp Container {id, inspectOutput} =
  case inspectOutput
    ^? Optics.key "NetworkSettings"
    % Optics.key "IPAddress"
    % Optics._String of
    Nothing ->
      throw $ InspectOutputUnexpected {id}
    Just address ->
      address

-- | Get the container's network alias.
-- Takes the first alias found.
--
-- @since 0.4.0.0
containerAlias :: Container -> Text
containerAlias Container {id, inspectOutput} =
  case inspectOutput
    ^? pre
      ( Optics.key "NetworkSettings"
          % Optics.key "Networks"
          % Optics.members
          % Optics.key "Aliases"
          % Optics.values
          % Optics._String
      ) of
    Nothing ->
      throw $
        InspectOutputMissingNetwork
          { id
          }
    Just alias ->
      alias

-- | Get the IP address for the container's gateway, i.e. the host.
-- Takes the first gateway address found.
--
-- @since 0.4.0.0
containerGateway :: Container -> Text
containerGateway Container {id, inspectOutput} =
  case inspectOutput
    ^? pre
      ( Optics.key "NetworkSettings"
          % Optics.key "Networks"
          % Optics.members
          % Optics.key "Gateway"
          % Optics._String
      ) of
    Nothing ->
      throw $
        InspectOutputMissingNetwork
          { id
          }
    Just gatewayIp ->
      gatewayIp

-- | Looks up an exposed port on the host.
--
-- @since 0.1.0.0
containerPort :: Container -> Int -> Int
containerPort Container {id, inspectOutput} port =
  let -- TODO also support UDP ports
      -- Using IsString so it works both with Text (aeson<2) and Aeson.Key (aeson>=2)
      textPort :: (IsString s) => s
      textPort = fromString $ show port <> "/tcp"
   in -- TODO be more mindful, make sure to grab the
      -- port from the right host address

      case inspectOutput
        ^? pre
          ( Optics.key "NetworkSettings"
              % Optics.key "Ports"
              % Optics.key textPort
              % Optics.values
              % Optics.key "HostPort"
              % Optics._String
          ) of
        Nothing ->
          throw $
            UnknownPortMapping
              { id,
                port = textPort
              }
        Just hostPort ->
          read (unpack hostPort)

-- | Returns the domain and port exposing the given container's port. Differs
-- from 'containerPort' in that 'containerAddress' will return the container's
-- domain and port if the program is running in the same network. Otherwise,
-- 'containerAddress' will use the exposed port on the Docker host.
--
-- @since 0.4.0.0
containerAddress :: (MonadIO m) => Container -> Int -> m (Text, Int)
containerAddress container port = do
  inDocker <- isRunningInDocker
  pure $
    if inDocker
      then (containerAlias container, port)
      else ("localhost", containerPort container port)

-- | Runs the `docker inspect` command. Memoizes the result.
--
-- @since 0.1.0.0
inspect :: (MonadDocker m) => Container -> m InspectOutput
inspect Container {inspectOutput} =
  pure inspectOutput

-- | Runs the `docker inspect` command.
--
-- @since 0.1.0.0
internalInspect :: (MonadThrow m, MonadIO m) => Tracer -> ContainerId -> m InspectOutput
internalInspect tracer id = do
  stdout <- docker tracer ["inspect", id]
  case decode' (LazyByteString.pack stdout) of
    Nothing ->
      throwM $ InspectOutputInvalidJSON {id}
    Just [] ->
      throwM $ InspectUnknownContainerId {id}
    Just [value] ->
      pure value
    Just _ ->
      Prelude.error "Internal: Multiple results where I expected single result"

askTracer :: (MonadReader Config m) => m Tracer
askTracer = do
  Config {configTracer} <- ask
  pure configTracer
{-# INLINE askTracer #-}

dockerHostOs :: (MonadDocker m) => m Text
dockerHostOs = do
  tracer <- askTracer
  strip . pack <$> docker tracer ["version", "--format", "{{.Server.Os}}"]

isDockerOnLinux :: (MonadDocker m) => m Bool
isDockerOnLinux =
  ("linux" ==) <$> dockerHostOs

-- | Detects if we are actually running in a Docker container.
isRunningInDocker :: (MonadIO m) => m Bool
isRunningInDocker = liftIO $ doesFileExist "/.dockerenv"
