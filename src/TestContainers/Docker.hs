{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module TestContainers.Docker
  (
    MonadDocker

  -- * Configuration

  , Config(..)
  , defaultDockerConfig
  , dockerForMacConfig
  , determineConfig

  -- * Docker image

  , ImageTag

  , Image
  , imageTag

  -- * Docker container

  , ContainerId
  , Container

  , containerId
  , containerImage
  , containerIp
  , containerPort
  , containerReleaseKey

  -- * Referring to images

  , ToImage

  , fromTag
  , fromBuildContext
  , fromDockerfile

  , build

  -- * Exceptions

  , DockerException(..)

  -- * Running containers

  , ContainerRequest
  , containerRequest
  , setName
  , setCmd
  , setRm
  , setEnv
  , setLink
  , setExpose
  , setWaitingFor
  , run

  -- * Managing the container lifecycle

  , InspectOutput
  , inspect

  , stop
  , kill
  , rm
  , withLogs

  -- * Wait for containers to become ready
  , WaitUntilReady
  , waitUntilReady

  -- * Only block for defined amounts of time
  , TimeoutException(..)
  , waitUntilTimeout

  -- * Wait until a specific pattern appears in the logs
  , waitWithLogs
  , UnexpectedEndOfPipe(..)
  , Pipe(..)
  , waitForLogLine

  -- * Misc. Docker functions

  , dockerHostOs
  , isDockerOnLinux

  -- * Wait until a socket is reachable
  , waitUntilMappedPortReachable

  -- * Reexports for convenience
  , ResIO
  , runResourceT

  , (&)
  ) where

import           Control.Concurrent           (threadDelay)
import           Control.Exception            (IOException, throw)
import           Optics.Operators             ((^?))
import           Optics.Optic                 ((%))
import           Optics.Fold                  (pre)
import           Control.Monad.Catch          (Exception, MonadCatch, MonadMask,
                                               MonadThrow, bracket, throwM, try)
import           Control.Monad.Fix            (mfix)
import           Control.Monad.IO.Class       (MonadIO (liftIO))
import           Control.Monad.IO.Unlift      (MonadUnliftIO (withRunInIO))
import           Control.Monad.Reader         (MonadReader (..), runReaderT)
import           Control.Monad.Trans.Resource (MonadResource (liftResourceT),
                                               ReleaseKey, ResIO, register,
                                               runResourceT)
import           Data.Aeson                   (Value, decode')
import qualified Data.Aeson.Optics            as Optics
import qualified Data.ByteString.Lazy.Char8   as LazyByteString
import           Data.Function                ((&))
import           Data.List                    (find)
import           Data.Text                    (Text, pack, strip, unpack)
import           Data.Text.Encoding.Error     (lenientDecode)
import qualified Data.Text.Lazy               as LazyText
import qualified Data.Text.Lazy.Encoding      as LazyText
import           GHC.Stack                    (HasCallStack,
                                               withFrozenCallStack)
import qualified Network.Socket               as Socket
import           Prelude                      hiding (error, id)
import qualified Prelude
import           System.Exit                  (ExitCode (..))
import           System.IO                    (Handle, hClose)
import           System.IO.Unsafe             (unsafePerformIO)
import qualified System.Process               as Process
import           System.Timeout               (timeout)


-- | Configuration for defaulting behavior.
--
-- Note that IP address returned by `containerIp` is not reachable when using
-- Docker For Mac (See https://docs.docker.com/docker-for-mac/networking/#per-container-ip-addressing-is-not-possible).
--
-- If you are targeting Docker For Mac you should use `dockerForMacConfig`, instead
-- use `defaultDockerConfig`.
--
-- @since 0.2.0.0
--
data Config = Config
  {
    -- | How to retrieve the IP address of a Docker container.
    -- There some known limitations around Docker for Mac in that
    -- it doesn't support accessing containers by their IP (see `containerIp`).
    --
    -- This configuration let's you define how to access the IP.
    configContainerIp :: Container -> Text
  }


-- | Default configuration.
--
-- @since 0.2.0.0
--
defaultDockerConfig :: Config
defaultDockerConfig = Config
  {
    configContainerIp = internalContainerIp
  }


-- | A default configuration to use with Docker for Mac installations. It doesn't
-- use a Docker container's IP address to access a container, instead it always uses
-- the loopback interface @0.0.0.0@.
--
-- @since 0.2.0.0
--
dockerForMacConfig :: Config
dockerForMacConfig = defaultDockerConfig
  {
    configContainerIp = const "0.0.0.0"
  }


-- | Autoselect the default configuration depending on wether you use Docker For
-- Mac or not.
determineConfig :: IO Config
determineConfig = do
  dockerOnLinux <- runResourceT isDockerOnLinux
  dho <- runResourceT dockerHostOs
  pure $ if dockerOnLinux then defaultDockerConfig else dockerForMacConfig


-- | Failing to interact with Docker results in this exception
-- being thrown.
--
-- @since 0.1.0.0
--
data DockerException
  = DockerException
    {
      -- | Exit code of the underlying Docker process.
      exitCode :: ExitCode
      -- | Arguments that were passed to Docker.
    , args     :: [Text]
      -- | Docker's STDERR output.
    , stderr   :: Text
    }
  | InspectUnknownContainerId { id :: ContainerId }
  | InspectOutputInvalidJSON  { id :: ContainerId }
  | InspectOutputUnexpected   { id :: ContainerId }
  | UnknownPortMapping
    {
      -- | Id of the `Container` that we tried to lookup the
      -- port mapping.
      id   :: ContainerId
      -- | Textual representation of port mapping we were
      -- trying to look up.
    , port :: Text
    }
  deriving (Eq, Show)


instance Exception DockerException


-- | Docker related functionality is parameterized over this `Monad`.
--
-- @since 0.1.0.0
--
type MonadDocker m =
  (MonadIO m, MonadMask m, MonadThrow m, MonadCatch m, MonadResource m, MonadReader Config m)


-- | Parameters for a running a Docker container.
--
-- @since 0.1.0.0
--
data ContainerRequest = ContainerRequest
  {
    toImage      :: ToImage
  , cmd          :: Maybe [Text]
  , env          :: [(Text, Text)]
  , exposedPorts :: [Int]
  , volumeMounts :: [(Text, Text)]
  , links        :: [ContainerId]
  , name         :: Maybe Text
  , rmOnExit     :: Bool
  , readiness    :: Maybe WaitUntilReady
  }


-- | Default `ContainerRequest`. Used as base for every Docker container.
--
-- @since 0.1.0.0
--
containerRequest :: ToImage -> ContainerRequest
containerRequest image = ContainerRequest
  {
    toImage      = image
  , name         = Nothing
  , cmd          = Nothing
  , env          = []
  , exposedPorts = []
  , volumeMounts = []
  , links        = []
  , rmOnExit     = True
  , readiness    = Nothing
  }


-- | Set the name of a Docker container. This is equivalent to invoking @docker run@
-- with the @--name@ parameter.
--
-- @since 0.1.0.0
--
setName :: Text -> ContainerRequest -> ContainerRequest
setName newName req =
  -- TODO error on empty Text
  req { name = Just newName }


-- | The command to execute inside the Docker container. This is the equivalent
-- of passing the command on the @docker run@ invocation.
--
-- @since 0.1.0.0
--
setCmd :: [Text] -> ContainerRequest -> ContainerRequest
setCmd newCmd req =
  req { cmd = Just newCmd }


-- | Wether to remove the container once exited. This is equivalent to passing
-- @--rm@ to @docker run@. (default is `True`).
--
-- @since 0.1.0.0
--
setRm :: Bool -> ContainerRequest -> ContainerRequest
setRm newRm req =
  req { rmOnExit = newRm }


-- | Set the environment for the container. This is equivalent to passing @--env key=value@
-- to @docker run@.
--
-- @since 0.1.0.0
--
setEnv :: [(Text, Text)] -> ContainerRequest -> ContainerRequest
setEnv newEnv req =
  req { env = newEnv }


-- | Set link on the container. This is equivalent to passing @--link other_container@
-- to @docker run@.
--
-- @since 0.1.0.0
--
setLink :: [ContainerId] -> ContainerRequest -> ContainerRequest
setLink newLink req =
  req { links = newLink }


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
--
setExpose :: [Int] -> ContainerRequest -> ContainerRequest
setExpose newExpose req =
  req { exposedPorts = newExpose }


-- | Set the waiting strategy on the container. Depending on a Docker container
-- it can take some time until the provided service is ready. You will want to
-- use to `setWaitingFor` to block until the container is ready to use.
--
-- @since 0.1.0.0
--
setWaitingFor :: WaitUntilReady -> ContainerRequest -> ContainerRequest
setWaitingFor newWaitingFor req =
  req { readiness = Just newWaitingFor }


-- | Runs a Docker container from an `Image` and `ContainerRequest`. A finalizer
-- is registered so that the container is aways stopped when it goes out of scope.
-- This function is essentially @docker run@.
--
-- @since 0.1.0.0
--
run :: MonadDocker m => ContainerRequest -> m Container
run request = do

  let
    ContainerRequest
      {
        toImage
      , name
      , cmd
      , env
      , exposedPorts
      , volumeMounts
      , links
      , rmOnExit
      , readiness
      } = request

  image@Image{ tag } <- runToImage toImage

  let
    dockerRun :: [Text]
    dockerRun = concat $
      [ [ "run" ] ] ++
      [ [ "--detach" ] ] ++
      [ [ "--name", containerName ] | Just containerName <- [name] ] ++
      [ [ "--env", variable <> "=" <> value  ] | (variable, value) <- env ] ++
      [ [ "--publish", pack (show port)] | port <- exposedPorts ] ++
      [ [ "--link", container ] | container <- links ] ++
      [ [ "--volume", src <> ":" <> dest ] | (src, dest) <- volumeMounts ] ++
      [ [ "--rm" ] | rmOnExit ] ++
      [ [ tag ] ] ++
      [ command | Just command <- [cmd] ]

  stdout <- docker dockerRun

  let
    id :: ContainerId
    !id =
      -- N.B. Force to not leak STDOUT String
      strip (pack stdout)

    -- Careful, this is really meant to be lazy
    ~inspectOutput = unsafePerformIO $
      internalInspect id

  config <- ask
  container <- liftResourceT $ mfix $ \container -> do
      -- Note: We have to tie the knot as the resulting container
      -- carries the release key as well.
      releaseKey <- register $ runReaderT (runResourceT (stop container)) config
      pure $ Container
        {
          id
        , releaseKey
        , image
        , inspectOutput
        , config
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
--
docker :: MonadIO m => [Text] -> m String
docker args =
  dockerWithStdin args ""


-- | Internal function that runs Docker. Takes care of throwing an exception
-- in case of failure.
--
-- @since 0.1.0.0
--
dockerWithStdin :: MonadIO m => [Text] -> Text -> m String
dockerWithStdin args stdin = liftIO $ do
  (exitCode, stdout, stderr) <- Process.readProcessWithExitCode "docker"
    (map unpack args) (unpack stdin)

  case exitCode of
    ExitSuccess -> pure stdout
    _ -> throwM $ DockerException
      {
        exitCode, args
      , stderr = pack stderr
      }


-- | Kills a Docker container. `kill` is essentially @docker kill@.
--
-- @since 0.1.0.0
--
kill :: MonadDocker m => Container -> m ()
kill Container { id } = do
  _ <- docker [ "kill", id ]
  return ()


-- | Stops a Docker container. `stop` is essentially @docker stop@.
--
-- @since 0.1.0.0
--
stop :: MonadDocker m => Container -> m ()
stop Container { id } = do
  _ <- docker [ "stop", id ]
  return ()


-- | Remove a Docker container. `rm` is essentially @docker rm -f@
--
-- @since 0.1.0.0
--
rm :: MonadDocker m => Container -> m ()
rm Container { id } = do
  _ <- docker [ "rm", "-f", "-v", id ]
  return ()


-- | Access STDOUT and STDERR of a running Docker container. This is essentially
-- @docker logs@ under the hood.
--
-- @since 0.1.0.0
--
withLogs :: forall m a . MonadDocker m => Container -> (Handle -> Handle -> m a) -> m a
withLogs Container { id } logger = do

  let
    acquire :: m (Handle, Handle, Handle, Process.ProcessHandle)
    acquire =
      liftIO $ Process.runInteractiveProcess
        "docker"
        [ "logs", "--follow", unpack id ]
        Nothing
        Nothing

    release :: (Handle, Handle, Handle, Process.ProcessHandle) -> m ()
    release (stdin, stdout, stderr, handle) =
      liftIO $ Process.cleanupProcess
        (Just stdin, Just stdout, Just stderr, handle)

  bracket acquire release $ \(stdin, stdout, stderr, _handle) -> do
    -- No need to keep it around...
    liftIO $ hClose stdin
    logger stdout stderr


-- | A tag to a Docker image.
--
-- @since 0.1.0.0
--
type ImageTag = Text


-- | A description of how to build an `Image`.
--
-- @since 0.1.0.0
--
data ToImage = ToImage
  {
    runToImage              :: forall m. MonadDocker m => m Image
  , applyToContainerRequest :: ContainerRequest -> ContainerRequest
  }


-- | Build the `Image` referred to by the argument. If the construction of the
-- image is expensive (e.g. a call to `fromBuildContext`) we don't want to
-- repeatedly build the image. Instead, `build` can be used to execute the
-- underlying Docker build once and re-use the resulting `Image`.
--
-- @since 0.1.0.0
--
build :: MonadDocker m => ToImage -> m ToImage
build toImage@ToImage { applyToContainerRequest } = do
  image <- runToImage toImage
  return $ ToImage
    {
      runToImage = pure image
    , applyToContainerRequest
    }


-- | Default `ToImage`. Doesn't apply anything to to `ContainerRequests`.
--
-- @since 0.1.0.0
--
defaultToImage :: (forall m . MonadDocker m => m Image) -> ToImage
defaultToImage action = ToImage
  {
    runToImage = action
  , applyToContainerRequest = \x -> x
  }


-- | Get an `Image` from a tag.
--
-- @since 0.1.0.0
--
fromTag :: ImageTag -> ToImage
fromTag tag = defaultToImage $ do
  output <- docker [ "pull", "--quiet", tag ]
  return $ Image
    {
      tag = strip (pack output)
    }


-- | Build the image from a build path and an optional path to the
-- Dockerfile (default is Dockerfile)
--
-- @since 0.1.0.0
--
fromBuildContext
  :: FilePath
  -> Maybe FilePath
  -> ToImage
fromBuildContext path mdockerfile = defaultToImage $ do
  let
    args
      | Just dockerfile <- mdockerfile =
          [ "build", "-f", pack dockerfile, pack path ]
      | otherwise =
          [ "build", pack path ]
  output <- docker args
  return $ Image
    {
      tag = strip (pack output)
    }


-- | Build a contextless image only from a Dockerfile passed as `Text`.
--
-- @since 0.1.0.0
--
fromDockerfile
  :: Text
  -> ToImage
fromDockerfile dockerfile = defaultToImage $ do
  output <- dockerWithStdin [ "build", "--quiet", "-" ] dockerfile
  return $ Image
    {
      tag = strip (pack output)
    }


-- | Identifies a container within the Docker runtime. Assigned by @docker run@.
--
-- @since 0.1.0.0
--
type ContainerId = Text


-- | Dumb logger abstraction to allow us to trace container execution.
--
-- @since 0.1.0.0
--
data Logger = Logger
  {
    debug :: forall m . (HasCallStack, MonadIO m) => Text -> m ()
  , info  :: forall m . (HasCallStack, MonadIO m) => Text -> m ()
  , warn  :: forall m . (HasCallStack, MonadIO m) => Text -> m ()
  , error :: forall m . (HasCallStack, MonadIO m) => Text -> m ()
  }


-- | Logger that doesn't log anything.
--
-- @since 0.1.0.0
--
silentLogger :: Logger
silentLogger = Logger
  {
    debug = \_ -> pure ()
  , info  = \_ -> pure ()
  , warn  = \_ -> pure ()
  , error = \_ -> pure ()
  }


-- | A strategy that describes how to asses readiness of a `Container`. Allows
-- Users to plug in their definition of readiness.
--
-- @since 0.1.0.0
--
newtype WaitUntilReady = WaitUntilReady
  {
    checkContainerReady :: Logger -> Config -> Container -> ResIO ()
  }


-- | The exception thrown by `waitForLine` in case the expected log line
-- wasn't found.
--
-- @since 0.1.0.0
--
newtype UnexpectedEndOfPipe = UnexpectedEndOfPipe
  {
    -- | The id of the underlying container.
    id :: ContainerId
  }
  deriving (Eq, Show)


instance Exception UnexpectedEndOfPipe


-- | The exception thrown by `waitUntilTimeout`.
--
-- @since 0.1.0.0
--
newtype TimeoutException = TimeoutException
  {
    -- | The id of the underlying container that was not ready in time.
    id :: ContainerId
  }
  deriving (Eq, Show)


instance Exception TimeoutException


-- | @waitUntilTimeout n waitUntilReady@ waits @n@ seconds for the container
-- to be ready. If the container is not ready by then a `TimeoutException` will
-- be thrown.
--
-- @since 0.1.0.0
--
waitUntilTimeout :: Int -> WaitUntilReady -> WaitUntilReady
waitUntilTimeout seconds wait = WaitUntilReady $ \logger config container@Container{ id } -> do
  withRunInIO $ \runInIO -> do
    result <- timeout (seconds * 1000000) $
      runInIO (checkContainerReady wait logger config container)
    case result of
      Nothing ->
        throwM $ TimeoutException { id }
      Just _ ->
        pure ()


-- | Waits until the port of a container is ready to accept connections.
-- This combinator should always be used with `waitUntilTimeout`.
--
-- @since 0.1.0.0
--
waitUntilMappedPortReachable
  :: Int
  -> WaitUntilReady
waitUntilMappedPortReachable port = WaitUntilReady $ \logger _config container ->
  withFrozenCallStack $ do

  let
    -- TODO add a parameterizable function when we will support host
    -- mapping exposure
    hostIp :: String
    hostIp = "0.0.0.0"

    hostPort :: String
    hostPort = show $ containerPort container port

    resolve = do
      let hints = Socket.defaultHints { Socket.addrSocketType = Socket.Stream }
      head <$> Socket.getAddrInfo (Just hints) (Just hostIp) (Just hostPort)

    open addr = do
      socket <- Socket.socket
        (Socket.addrFamily addr)
        (Socket.addrSocketType addr)
        (Socket.addrProtocol addr)
      Socket.connect
        socket
        (Socket.addrAddress addr)
      pure socket

    retry = do
      debug logger $ "Trying to open socket to " <> pack hostIp <> ":" <> pack hostPort
      result <- try (resolve >>= open)
      case result of
        Right socket -> do
          debug logger $ "Successfully opened socket to " <> pack hostIp <> ":" <> pack hostPort
          Socket.close socket
          pure ()

        Left (exception :: IOException) -> do
          debug logger $ "Failed to open socket to " <> pack hostIp <> ":" <>
            pack hostPort <> " with " <> pack (show exception)
          threadDelay 500000
          retry

  liftIO retry


-- | A low-level primitive that allows scanning the logs for specific log lines
-- that indicate readiness of a container.
--
-- The `Handle`s passed to the function argument represent @stdout@ and @stderr@
-- of the container.
--
-- @since 0.1.0.0
--
waitWithLogs :: (Container -> Handle -> Handle -> IO ()) -> WaitUntilReady
waitWithLogs waiter = WaitUntilReady $ \_logger config container -> do
  flip runReaderT config $ withLogs container $ \stdout stderr ->
    liftIO $ waiter container stdout stderr


-- | A data type indicating which pipe to scan for a specific log line.
--
-- @since 0.1.0.0
--
data Pipe
  -- | Refer to logs on STDOUT.
  = Stdout
  -- | Refer to logs on STDERR.
  | Stderr
  deriving (Eq, Ord, Show)


-- | Waits for a specific line to occur in the logs. Throws a `UnexpectedEndOfPipe`
-- exception in case the desired line can not be found on the logs.
--
-- Say you want to find "Ready to accept connections" in the logs on Stdout try:
--
-- @
-- wairForLogLine Stdout ("Ready to accept connections" ``LazyText.isInfixOf``)
-- @
--
-- @since 0.1.0.0
--
waitForLogLine :: Pipe -> (LazyText.Text -> Bool) -> WaitUntilReady
waitForLogLine whereToLook matches = waitWithLogs $ \Container { id } stdout stderr -> do
  let
    logs :: Handle
    logs = case whereToLook of
      Stdout -> stdout
      Stderr -> stderr

  logContent <- LazyByteString.hGetContents logs

  let
    logLines :: [LazyText.Text]
    logLines =
      -- FIXME: This is assuming UTF8 encoding. Do better!
      map
        (LazyText.decodeUtf8With lenientDecode)
        (LazyByteString.lines logContent)

  case find matches logLines of
    Just _  -> pure ()
    Nothing -> throwM $ UnexpectedEndOfPipe { id }


-- | Blocks until the container is ready. `waitUntilReady` might throw exceptions
-- depending on the used `WaitUntilReady` on the container.
--
-- @since 0.1.0.0
--
waitUntilReady :: MonadDocker m => Container -> WaitUntilReady -> m ()
waitUntilReady container waiter = do
  config <- ask
  liftResourceT $ checkContainerReady waiter silentLogger config container


-- | Handle to a Docker image.
--
-- @since 0.1.0.0
--
data Image = Image
  {
    -- | The image tag assigned by Docker. Uniquely identifies an `Image`
    -- within Docker.
    tag :: ImageTag
  }
  deriving (Eq, Show)


-- | The image tag assigned by Docker. Uniquely identifies an `Image`
-- within Docker.
--
-- @since 0.1.0.0
--
imageTag :: Image -> ImageTag
imageTag Image { tag } = tag


-- | Handle to a Docker container.
--
-- @since 0.1.0.0
--
data Container = Container
  {
    -- | The container Id assigned by Docker, uniquely identifying this `Container`.
    id            :: ContainerId
    -- | Underlying `ReleaseKey` for the resource finalizer.
  , releaseKey    :: ReleaseKey
    -- | The underlying `Image` of this container.
  , image         :: Image
    -- | Configuration used to create and run this container.
  , config        :: Config
    -- | Memoized output of `docker inspect`. This is being calculated lazily.
  , inspectOutput :: InspectOutput
  }


-- | The parsed JSON output of docker inspect command.
--
-- @since 0.1.0.0
--
type InspectOutput = Value


-- | Returns the id of the container.
--
-- @since 0.1.0.0
--
containerId :: Container -> ContainerId
containerId Container { id } = id


-- | Returns the underlying image of the container.
--
-- @since 0.1.0.0
--
containerImage :: Container -> Image
containerImage Container { image } = image


-- | Returns the internal release key used for safely shutting down
-- the container. Use this with care. This function is considered
-- an internal detail.
--
-- @since 0.1.0.0
--
containerReleaseKey :: Container -> ReleaseKey
containerReleaseKey Container { releaseKey } = releaseKey


-- | Looks up the ip address of the container.
--
-- @since 0.1.0.0
--
containerIp :: Container -> Text
containerIp container@Container { config = Config { configContainerIp } } =
  configContainerIp container


-- | Get the IP address of a running Docker container using @docker inspect@.
internalContainerIp :: Container -> Text
internalContainerIp Container { id, inspectOutput } =
  case inspectOutput
    ^? Optics.key "NetworkSettings"
    % Optics.key "IPAddress"
    % Optics._String of

    Nothing ->
      throw $ InspectOutputUnexpected { id }

    Just address ->
      address


-- | Looks up an exposed port on the host.
--
-- @since 0.1.0.0
--
containerPort :: Container -> Int -> Int
containerPort Container { id, inspectOutput } port =
  let
    -- TODO also support UDP ports
    textPort :: Text
    textPort = pack (show port) <> "/tcp"
  in
    -- TODO be more mindful, make sure to grab the
    -- port from the right host address

    case inspectOutput
    ^? pre (Optics.key "NetworkSettings"
           % Optics.key "Ports"
           % Optics.key textPort
           % Optics.values
           % Optics.key "HostPort"
           % Optics._String) of

      Nothing ->
        throw $ UnknownPortMapping
          {
            id
          , port = textPort
          }
      Just hostPort ->
        read (unpack hostPort)


-- | Runs the `docker inspect` command. Memoizes the result.
--
-- @since 0.1.0.0
--
inspect :: MonadDocker m => Container -> m InspectOutput
inspect Container { inspectOutput } =
  pure inspectOutput


-- | Runs the `docker inspect` command.
--
-- @since 0.1.0.0
--
internalInspect :: (MonadThrow m, MonadIO m) => ContainerId -> m InspectOutput
internalInspect id = do
  stdout <- docker [ "inspect", id ]
  case decode' (LazyByteString.pack stdout) of
    Nothing ->
      throwM $ InspectOutputInvalidJSON { id }
    Just [] ->
      throwM $ InspectUnknownContainerId { id }
    Just [value] ->
      pure value
    Just _ ->
      Prelude.error "Internal: Multiple results where I expected single result"


dockerHostOs :: (MonadResource m, MonadMask m, MonadIO m) => m Text
dockerHostOs =
  strip . pack <$> docker [ "version", "--format", "{{.Server.Os}}" ]


isDockerOnLinux :: (MonadResource m, MonadMask m, MonadIO m) => m Bool
isDockerOnLinux =
  ("linux" ==) <$> dockerHostOs
