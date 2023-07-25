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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module TestContainers.Docker
  ( MonadDocker,
    TestContainer,

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

    -- * Port
    Port (..),

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

    -- * Container state
    State,
    Status (..),
    stateError,
    stateExitCode,
    stateFinishedAt,
    stateOOMKilled,
    statePid,
    stateStartedAt,
    stateStatus,

    -- * Predicates to assert container state
    successfulExit,

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
    withLabels,
    setName,
    setFixedName,
    setSuffixedName,
    setRandomName,
    setCmd,
    setVolumeMounts,
    setRm,
    setEnv,
    withWorkingDirectory,
    withNetwork,
    withNetworkAlias,
    setLink,
    setExpose,
    setWaitingFor,
    run,

    -- * Following logs
    LogConsumer,
    consoleLogConsumer,
    withFollowLogs,

    -- * Network related functionality
    NetworkId,
    Network,
    NetworkRequest,
    networkId,
    networkRequest,
    createNetwork,
    fromExistingNetwork,
    withIpv6,
    withDriver,

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

    -- * Wait for container state
    waitForState,

    -- * Wait until a specific pattern appears in the logs
    waitWithLogs,
    Pipe (..),
    UnexpectedEndOfPipe (..),
    waitForLogLine,

    -- * Misc. Docker functions
    dockerHostOs,
    isDockerOnLinux,

    -- * Wait until a socket is reachable
    waitUntilMappedPortReachable,

    -- * Wait until the http server responds with a specific status code
    waitForHttp,

    -- * Reaper
    createRyukReaper,

    -- * Reexports for convenience
    ResIO,
    runResourceT,
    (&),
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, throw)
import Control.Monad (forM_, replicateM, unless)
import Control.Monad.Catch
  ( Exception,
    MonadCatch,
    MonadThrow,
    bracket,
    throwM,
    try,
  )
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.IO.Unlift (MonadUnliftIO (withRunInIO))
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.Trans.Resource
  ( ReleaseKey,
    ResIO,
    register,
    runResourceT,
  )
import Data.Aeson (decode')
import qualified Data.Aeson.Optics as Optics
import qualified Data.ByteString.Lazy.Char8 as LazyByteString
import Data.Function ((&))
import Data.List (find)
import Data.String (IsString (..))
import Data.Text (Text, pack, splitOn, strip, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText
import Data.Text.Read (decimal)
import GHC.Stack (withFrozenCallStack)
import Network.HTTP.Client
  ( HttpException,
    Manager,
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
import System.IO (Handle, hClose)
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Process as Process
import qualified System.Random as Random
import System.Timeout (timeout)
import TestContainers.Config
  ( Config (..),
    defaultDockerConfig,
    determineConfig,
  )
import TestContainers.Docker.Internal
  ( ContainerId,
    DockerException (..),
    InspectOutput,
    LogConsumer,
    Pipe (..),
    consoleLogConsumer,
    docker,
    dockerFollowLogs,
    dockerWithStdin,
  )
import TestContainers.Docker.Network
  ( Network,
    NetworkId,
    NetworkRequest,
    createNetwork,
    fromExistingNetwork,
    networkId,
    networkRequest,
    withDriver,
    withIpv6,
  )
import TestContainers.Docker.Reaper
  ( Reaper,
    newRyukReaper,
    reaperLabels,
    ryukImageTag,
    ryukPort,
  )
import TestContainers.Docker.State
  ( State,
    Status (..),
    containerState,
    stateError,
    stateExitCode,
    stateFinishedAt,
    stateOOMKilled,
    statePid,
    stateStartedAt,
    stateStatus,
  )
import TestContainers.Monad
  ( MonadDocker,
    TestContainer,
  )
import TestContainers.Trace (Trace (..), Tracer, newTracer, withTrace)
import Prelude hiding (error, id)
import qualified Prelude

-- | Parameters for a running a Docker container.
--
-- @since 0.1.0.0
data ContainerRequest = ContainerRequest
  { toImage :: ToImage,
    cmd :: Maybe [Text],
    env :: [(Text, Text)],
    exposedPorts :: [Port],
    volumeMounts :: [(Text, Text)],
    network :: Maybe (Either Network Text),
    networkAlias :: Maybe Text,
    links :: [ContainerId],
    naming :: NamingStrategy,
    rmOnExit :: Bool,
    readiness :: WaitUntilReady,
    labels :: [(Text, Text)],
    noReaper :: Bool,
    followLogs :: Maybe LogConsumer,
    workDirectory :: Maybe Text
  }

-- | Parameters for a naming a Docker container.
--
-- @since 0.5.0.0
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
      networkAlias = Nothing,
      links = [],
      rmOnExit = False,
      readiness = mempty,
      labels = mempty,
      noReaper = False,
      followLogs = Nothing,
      workDirectory = Nothing
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
-- @since 0.5.0.0
setFixedName :: Text -> ContainerRequest -> ContainerRequest
setFixedName newName req =
  -- TODO error on empty Text
  req {naming = FixedName newName}

-- | Set the name randomly given of a Docker container. This is equivalent to omitting
--  the @--name@ parameter calling @docker run@.
--
-- @since 0.5.0.0
setRandomName :: ContainerRequest -> ContainerRequest
setRandomName req =
  -- TODO error on empty Text
  req {naming = RandomName}

-- | Set the name randomly suffixed of a Docker container. This is equivalent to invoking
-- @docker run@ with the @--name@ parameter.
--
-- @since 0.5.0.0
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

-- | Sets the working directory inside the container.
--
-- @since x.x.x
withWorkingDirectory :: Text -> ContainerRequest -> ContainerRequest
withWorkingDirectory workdir request =
  request {workDirectory = Just workdir}

-- | Set the network the container will connect to. This is equivalent to passing
-- @--network network_name@ to @docker run@.
--
-- @since 0.5.0.0
withNetwork :: Network -> ContainerRequest -> ContainerRequest
withNetwork network req =
  req {network = Just (Left network)}

-- | Set the network alias for this container. This is equivalent to passing
-- @--network-alias alias@ to @docker run@.
--
-- @since 0.5.0.0
withNetworkAlias :: Text -> ContainerRequest -> ContainerRequest
withNetworkAlias alias req =
  req {networkAlias = Just alias}

-- | Sets labels for a container
--
-- @since 0.5.0.0
withLabels :: [(Text, Text)] -> ContainerRequest -> ContainerRequest
withLabels xs request =
  request {labels = xs}

-- | Set link on the container. This is equivalent to passing @--link other_container@
-- to @docker run@.
--
-- @since 0.1.0.0
setLink :: [ContainerId] -> ContainerRequest -> ContainerRequest
setLink newLink req =
  req {links = newLink}

-- | Forwards container logs to the given 'LogConsumer' once ran.
--
-- @since 0.5.0.0
withFollowLogs :: LogConsumer -> ContainerRequest -> ContainerRequest
withFollowLogs logConsumer request =
  request {followLogs = Just logConsumer}

-- | Defintion of a 'Port'. Allows for specifying ports using various protocols. Due to the
-- 'Num' and 'IsString' instance allows for convenient Haskell literals.
--
-- >>> "80" :: Port
-- 80/tcp
--
-- >>> "80/tcp" :: Port
-- 80/tcp
--
-- >>> 80 :: Port
-- 80/tcp
--
-- >>> "90/udp" :: Port
-- 90/udp
data Port = Port
  { port :: Int,
    protocol :: Text
  }
  deriving stock (Eq, Ord)

defaultProtocol :: Text
defaultProtocol = "tcp"

-- @since 0.5.0.0
instance Show Port where
  show Port {port, protocol} =
    show port <> "/" <> unpack protocol

-- | A cursed but handy instance supporting literal 'Port's.
--
-- @since 0.5.0.0
instance Num Port where
  fromInteger x =
    Port {port = fromIntegral x, protocol = defaultProtocol}
  (+) = Prelude.error "not implemented"
  (*) = Prelude.error "not implemented"
  abs = Prelude.error "not implemented"
  signum = Prelude.error "not implemented"
  negate = Prelude.error "not implemented"

-- | A cursed but handy instance supporting literal 'Port's of them
-- form @"8080"@, @"8080/udp"@, @"8080/tcp"@.
--
-- @since 0.5.0.0
instance IsString Port where
  fromString input = case splitOn "/" (pack input) of
    [numberish]
      | Right (port, "") <- decimal numberish ->
          Port {port, protocol = defaultProtocol}
    [numberish, protocol]
      | Right (port, "") <- decimal numberish ->
          Port {port, protocol}
    _ ->
      Prelude.error ("invalid port literal: " <> input)

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
setExpose :: [Port] -> ContainerRequest -> ContainerRequest
setExpose newExpose req =
  req {exposedPorts = newExpose}

-- | Set the waiting strategy on the container. Depending on a Docker container
-- it can take some time until the provided service is ready. You will want to
-- use to `setWaitingFor` to block until the container is ready to use.
--
-- @since 0.1.0.0
setWaitingFor :: WaitUntilReady -> ContainerRequest -> ContainerRequest
setWaitingFor newWaitingFor req =
  req {readiness = newWaitingFor}

-- | Runs a Docker container from an `Image` and `ContainerRequest`. A finalizer
-- is registered so that the container is aways stopped when it goes out of scope.
-- This function is essentially @docker run@.
--
-- @since 0.1.0.0
run :: ContainerRequest -> TestContainer Container
run request = do
  let ContainerRequest
        { toImage,
          naming,
          cmd,
          env,
          exposedPorts,
          volumeMounts,
          network,
          networkAlias,
          links,
          rmOnExit,
          readiness,
          labels,
          noReaper,
          followLogs,
          workDirectory
        } = request

  config@Config {configTracer, configCreateReaper} <-
    ask

  additionalLabels <-
    if noReaper
      then do
        pure []
      else reaperLabels <$> configCreateReaper

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
            ++ [["--label", label <> "=" <> value] | (label, value) <- additionalLabels ++ labels]
            ++ [["--env", variable <> "=" <> value] | (variable, value) <- env]
            ++ [["--publish", pack (show port) <> "/" <> protocol] | Port {port, protocol} <- exposedPorts]
            ++ [["--network", networkName] | Just (Right networkName) <- [network]]
            ++ [["--network", networkId dockerNetwork] | Just (Left dockerNetwork) <- [network]]
            ++ [["--network-alias", alias] | Just alias <- [networkAlias]]
            ++ [["--link", container] | container <- links]
            ++ [["--volume", src <> ":" <> dest] | (src, dest) <- volumeMounts]
            ++ [["--rm"] | rmOnExit]
            ++ [["--workdir", workdir] | Just workdir <- [workDirectory]]
            ++ [[tag]]
            ++ [command | Just command <- [cmd]]

  stdout <- docker configTracer dockerRun

  let id :: ContainerId
      !id =
        -- N.B. Force to not leak STDOUT String
        strip (pack stdout)

      -- Careful, this is really meant to be lazy
      ~inspectOutput =
        unsafePerformIO $
          internalInspect configTracer id

  -- We don't issue 'ReleaseKeys' for cleanup anymore. Ryuk takes care of cleanup
  -- for us once the session has been closed.
  releaseKey <- register (pure ())

  forM_ followLogs $
    dockerFollowLogs configTracer id

  let container =
        Container
          { id,
            releaseKey,
            image,
            inspectOutput,
            config
          }

  -- Last but not least, execute the WaitUntilReady checks
  waitUntilReady container readiness

  pure container

-- | Sets up a Ryuk 'Reaper'.
--
-- @since 0.5.0.0
createRyukReaper :: TestContainer Reaper
createRyukReaper = do
  ryukContainer <-
    run $
      containerRequest (fromTag ryukImageTag)
        & skipReaper
        & setVolumeMounts [("/var/run/docker.sock", "/var/run/docker.sock")]
        & setExpose [ryukPort]
        & setWaitingFor (waitUntilMappedPortReachable ryukPort)
        & setRm True

  let (ryukContainerAddress, ryukContainerPort) =
        containerAddress ryukContainer ryukPort

  newRyukReaper ryukContainerAddress ryukContainerPort

-- | Internal attribute, serving as a loop breaker: When runnign a container
-- we ensure a 'Reaper' is present, since the 'Reaper' itself is a running
-- container we need to break the loop to avoid spinning up a new 'Reaper' for
-- the 'Reaper'.
skipReaper :: ContainerRequest -> ContainerRequest
skipReaper request =
  request {noReaper = True}

-- | Kills a Docker container. `kill` is essentially @docker kill@.
--
-- @since 0.1.0.0
kill :: Container -> TestContainer ()
kill Container {id} = do
  tracer <- askTracer
  _ <- docker tracer ["kill", id]
  return ()

-- | Stops a Docker container. `stop` is essentially @docker stop@.
--
-- @since 0.1.0.0
stop :: Container -> TestContainer ()
stop Container {id} = do
  tracer <- askTracer
  _ <- docker tracer ["stop", id]
  return ()

-- | Remove a Docker container. `rm` is essentially @docker rm -f@
--
-- @since 0.1.0.0
rm :: Container -> TestContainer ()
rm Container {id} = do
  tracer <- askTracer
  _ <- docker tracer ["rm", "-f", "-v", id]
  return ()

-- | Access STDOUT and STDERR of a running Docker container. This is essentially
-- @docker logs@ under the hood.
--
-- @since 0.1.0.0
withLogs :: Container -> (Handle -> Handle -> TestContainer a) -> TestContainer a
withLogs Container {id} logger = do
  let acquire :: TestContainer (Handle, Handle, Handle, Process.ProcessHandle)
      acquire =
        liftIO $
          Process.runInteractiveProcess
            "docker"
            ["logs", "--follow", unpack id]
            Nothing
            Nothing

      release :: (Handle, Handle, Handle, Process.ProcessHandle) -> TestContainer ()
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
  { runToImage :: TestContainer Image
  }

-- | Build the `Image` referred to by the argument. If the construction of the
-- image is expensive (e.g. a call to `fromBuildContext`) we don't want to
-- repeatedly build the image. Instead, `build` can be used to execute the
-- underlying Docker build once and re-use the resulting `Image`.
--
-- @since 0.1.0.0
build :: ToImage -> TestContainer ToImage
build toImage = do
  image <- runToImage toImage
  return $
    toImage
      { runToImage = pure image
      }

-- | Default `ToImage`. Doesn't apply anything to to `ContainerRequests`.
--
-- @since 0.1.0.0
defaultToImage :: TestContainer Image -> ToImage
defaultToImage action =
  ToImage
    { runToImage = action
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
            ["build", "--quiet", "--file", pack dockerfile, pack path]
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

-- | A strategy that describes how to asses readiness of a `Container`. Allows
-- Users to plug in their definition of readiness.
--
-- @since 0.1.0.0
data WaitUntilReady
  = -- | A blocking action that attests readiness
    WaitReady
      -- Check to run
      (Container -> TestContainer ())
  | -- | In order to keep readiness checking at bay this node
    -- ensures checks are not exceeding their time share
    WaitUntilTimeout
      -- Timeout in seconds
      Int
      -- Action to watch with with timeout
      WaitUntilReady
  | WaitMany
      -- First check
      WaitUntilReady
      -- Next check
      WaitUntilReady

-- | @since 0.5.0.0
instance Semigroup WaitUntilReady where
  (<>) = WaitMany

-- | @since 0.5.0.0
instance Monoid WaitUntilReady where
  mempty = WaitReady mempty

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

-- | The exception thrown by `waitForState`.
--
-- @since 0.1.0.0
newtype InvalidStateException = InvalidStateException
  { -- | The id of the underlying container that was not ready in time.
    id :: ContainerId
  }
  deriving stock (Eq, Show)

instance Exception InvalidStateException

-- | @waitForState@ waits for a certain state of the container. If the container reaches a terminal
-- state 'InvalidStateException' will be thrown.
--
-- @since 0.5.0.0
waitForState :: (State -> Bool) -> WaitUntilReady
waitForState isReady = WaitReady $ \Container {id} -> do
  let wait = do
        Config {configTracer} <-
          ask
        inspectOutput <-
          internalInspect configTracer id

        let state = containerState inspectOutput

        if isReady state
          then pure ()
          else do
            case stateStatus state of
              Exited ->
                -- Once exited, state won't change!
                throwM InvalidStateException {id}
              Dead ->
                -- Once dead, state won't change!
                throwM InvalidStateException {id}
              _ -> do
                liftIO (threadDelay 500000)
                wait
  wait

-- | @successfulExit@ is supposed to be used in conjunction with 'waitForState'.
--
-- @since 0.5.0.0
successfulExit :: State -> Bool
successfulExit state =
  stateStatus state == Exited && stateExitCode state == Just 0

-- | @waitUntilTimeout n waitUntilReady@ waits @n@ seconds for the container
-- to be ready. If the container is not ready by then a `TimeoutException` will
-- be thrown.
--
-- @since 0.1.0.0
waitUntilTimeout :: Int -> WaitUntilReady -> WaitUntilReady
waitUntilTimeout = WaitUntilTimeout

-- | Waits for a specific http status code.
-- This combinator should always be used with `waitUntilTimeout`.
--
-- @since 0.5.0.0
waitForHttp ::
  -- | Port
  Port ->
  -- | URL path
  String ->
  -- | Acceptable status codes
  [Int] ->
  WaitUntilReady
waitForHttp port path acceptableStatusCodes = WaitReady $ \container -> do
  Config {configTracer} <- ask
  let wait :: (MonadIO m, MonadCatch m) => m ()
      wait =
        liftIO (newManager defaultManagerSettings) >>= retry

      retry :: (MonadIO m, MonadCatch m) => Manager -> m ()
      retry manager = do
        let (endpointHost, endpointPort) =
              containerAddress container port
        let request =
              defaultRequest
                { host = encodeUtf8 endpointHost,
                  port = endpointPort,
                  path = encodeUtf8 (pack path)
                }
        result <-
          try $
            statusCode . responseStatus <$> liftIO (httpNoBody request manager)
        case result of
          Right code -> do
            withTrace
              configTracer
              (TraceHttpCall endpointHost endpointPort (Right code))
            unless (code `elem` acceptableStatusCodes) $ do
              liftIO (threadDelay 500000)
              retry manager
          Left (exception :: HttpException) -> do
            withTrace
              configTracer
              (TraceHttpCall endpointHost endpointPort (Left $ show exception))
            liftIO (threadDelay 500000)
            retry manager

  wait

-- | Waits until the port of a container is ready to accept connections.
-- This combinator should always be used with `waitUntilTimeout`.
--
-- @since 0.1.0.0
waitUntilMappedPortReachable ::
  Port ->
  WaitUntilReady
waitUntilMappedPortReachable port = WaitReady $ \container -> do
  withFrozenCallStack $ do
    Config {configTracer} <- ask

    let resolve endpointHost endpointPort = do
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

        wait = do
          let (endpointHost, endpointPort) =
                containerAddress container port

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
              wait

    liftIO wait

-- | A low-level primitive that allows scanning the logs for specific log lines
-- that indicate readiness of a container.
--
-- The `Handle`s passed to the function argument represent @stdout@ and @stderr@
-- of the container.
--
-- @since 0.1.0.0
waitWithLogs :: (Container -> Handle -> Handle -> IO ()) -> WaitUntilReady
waitWithLogs waiter = WaitReady $ \container ->
  withLogs container $ \stdout stderr ->
    liftIO $ waiter container stdout stderr

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
-- In case the readiness check times out 'waitUntilReady' throws a
-- 'TimeoutException'.
--
-- @since 0.1.0.0
waitUntilReady :: Container -> WaitUntilReady -> TestContainer ()
waitUntilReady container@Container {id} input = do
  Config {configDefaultWaitTimeout} <- ask
  interpreter $ case configDefaultWaitTimeout of
    Just seconds -> waitUntilTimeout seconds input
    Nothing -> input
  where
    interpreter :: WaitUntilReady -> TestContainer ()
    interpreter wait =
      case wait of
        WaitReady check ->
          check container
        WaitUntilTimeout seconds rest ->
          withRunInIO $ \runInIO -> do
            result <-
              timeout (seconds * 1000000) $
                runInIO (interpreter rest)
            case result of
              Nothing ->
                throwM $ TimeoutException {id}
              Just {} ->
                pure ()
        WaitMany first second -> do
          interpreter first
          interpreter second

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
{-# DEPRECATED containerReleaseKey "Containers are cleaned up with a separate resource reaper. Releasing the container manually is not going to work." #-}

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
-- @since 0.5.0.0
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
-- @since 0.5.0.0
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
containerPort :: Container -> Port -> Int
containerPort Container {id, inspectOutput} Port {port, protocol} =
  let -- TODO also support UDP ports
      -- Using IsString so it works both with Text (aeson<2) and Aeson.Key (aeson>=2)
      textPort :: (IsString s) => s
      textPort = fromString $ show port <> "/" <> unpack protocol
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
-- @since 0.5.0.0
containerAddress :: Container -> Port -> (Text, Int)
containerAddress container Port {port, protocol} =
  let inDocker = unsafePerformIO isRunningInDocker
   in if inDocker
        then (containerAlias container, port)
        else ("localhost", containerPort container (Port {port, protocol}))

-- | Runs the `docker inspect` command. Memoizes the result.
--
-- @since 0.1.0.0
inspect :: Container -> TestContainer InspectOutput
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

dockerHostOs :: TestContainer Text
dockerHostOs = do
  tracer <- askTracer
  strip . pack <$> docker tracer ["version", "--format", "{{.Server.Os}}"]

isDockerOnLinux :: TestContainer Bool
isDockerOnLinux =
  ("linux" ==) <$> dockerHostOs

-- | Detects if we are actually running in a Docker container.
isRunningInDocker :: (MonadIO m) => m Bool
isRunningInDocker = liftIO $ doesFileExist "/.dockerenv"
