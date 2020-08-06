{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module TestContainers.Docker
  (
    MonadDocker

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

  -- * Wait until a socket is reachable
  , waitUntilMappedPortReachable

  -- * Reexports for convenience
  , ResIO
  , runResourceT

  , (&)
  ) where

import           Control.Concurrent           (threadDelay)
import           Control.Exception            (IOException, throw)
import           Control.Lens                 ((^?))
import           Control.Monad.Catch          (Exception, MonadCatch, MonadMask,
                                               MonadThrow, bracket, throwM, try)
import           Control.Monad.Fix            (mfix)
import           Control.Monad.IO.Class       (MonadIO (liftIO))
import           Control.Monad.IO.Unlift      (MonadUnliftIO (withRunInIO))
import           Control.Monad.Trans.Resource (MonadResource (liftResourceT),
                                               ReleaseKey, ResIO, register,
                                               runResourceT)
import           Data.Aeson                   (Value, decode')
import qualified Data.Aeson.Lens              as Lens
import qualified Data.ByteString.Lazy.Char8   as LazyByteString
import           Data.Function                ((&))
import           Data.List                    (find)
import           Data.Text                    (Text, pack, strip, unpack)
import           Data.Text.Encoding.Error     (lenientDecode)
import qualified Data.Text.Lazy               as LazyText
import qualified Data.Text.Lazy.Encoding      as LazyText
import           GHC.Stack                    (HasCallStack, withFrozenCallStack)
import qualified Network.Socket               as Socket
import           Prelude                      hiding (error, id)
import qualified Prelude
import           System.Exit                  (ExitCode (..))
import           System.IO                    (Handle, hClose)
import           System.IO.Unsafe             (unsafePerformIO)
import qualified System.Process               as Process
import           System.Timeout               (timeout)


-- | Failing to interact with Docker results in this exception
-- being thrown.
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
      id            :: ContainerId
      -- | Textual representation of port mapping we were
      -- trying to look up.
    , port :: Text
    }
  deriving (Eq, Show)


instance Exception DockerException


-- | Docker related functionality is parameterized over this `Monad`.
type MonadDocker m =
  (MonadIO m, MonadMask m, MonadThrow m, MonadCatch m, MonadResource m)


-- | Parameters for a running a Docker container.
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


-- | Set the name of a Docker container.
setName :: Text -> ContainerRequest -> ContainerRequest
setName newName req =
  -- TODO error on empty Text
  req { name = Just newName }


-- | The command to execute inside the Docker container.
setCmd :: [Text] -> ContainerRequest -> ContainerRequest
setCmd newCmd req =
  req { cmd = Just newCmd }


-- | Wether to remove the container once exited.
setRm :: Bool -> ContainerRequest -> ContainerRequest
setRm newRm req =
  req { rmOnExit = newRm }


-- | Set the environment for the container.
setEnv :: [(Text, Text)] -> ContainerRequest -> ContainerRequest
setEnv newEnv req =
  req { env = newEnv }


-- | Set link on the container.
setLink :: [ContainerId] -> ContainerRequest -> ContainerRequest
setLink newLink req =
  req { links = newLink }


-- | Set exposed ports on the container.
--
-- Example:
--
-- @ redis
--     `&` `setExpose` [ 6379 ]
-- @
--
setExpose :: [Int] -> ContainerRequest -> ContainerRequest
setExpose newExpose req =
  req { exposedPorts = newExpose }


-- | Set the waiting strategy on the container.
setWaitingFor :: WaitUntilReady -> ContainerRequest -> ContainerRequest
setWaitingFor newWaitingFor req =
  req { readiness = Just newWaitingFor }


-- | Runs a Docker container from an `Image` and `ContainerRequest`. A finalizer
-- is registered so that the container is aways stopped when it goes out of scope.
run :: MonadDocker m => ContainerRequest -> m Container
run request = liftResourceT $ do

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

  let
    -- Careful, this is really meant to be lazy
    ~inspectOutput = unsafePerformIO $
      internalInspect id

  container <- mfix $ \container -> do
    -- Note: We have to tie the knot as the resulting container
    -- carries the release key as well.
    releaseKey <- register $ runResourceT (stop container)
    pure $ Container
      {
        id
      , releaseKey
      , image
      , inspectOutput
      }

  case readiness of
    Just wait ->
      waitUntilReady container wait
    Nothing ->
      pure ()

  pure container


-- | Internal function that runs Docker. Takes care of throwing an exception
-- in case of failure.
docker :: MonadIO m => [Text] -> m String
docker args =
  dockerWithStdin args ""


-- | Internal function that runs Docker. Takes care of throwing an exception
-- in case of failure.
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


-- | Kills a Docker container.
kill :: MonadDocker m => Container -> m ()
kill Container { id } = do
  _ <- docker [ "kill", id ]
  return ()


-- | Stops a Docker container.
stop :: MonadDocker m => Container -> m ()
stop Container { id } = do
  _ <- docker [ "stop", id ]
  return ()


-- | Remove a Docker container.
rm :: MonadDocker m => Container -> m ()
rm Container { id } = do
  _ <- docker [ "rm", "-f", "-v", id ]
  return ()


-- | Get the logs from a Docker container.
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
type ImageTag = Text


-- | A description of how to build an `Image`.
data ToImage = ToImage
  {
    runToImage              :: forall m. MonadDocker m => m Image
  , applyToContainerRequest :: ContainerRequest -> ContainerRequest
  }


-- | Build the `Image` referred to by the argument. If the construction of the
-- image is expensive (e.g. a call to `fromBuildContext`) we don't want to
-- repeatedly build the image. Instead, `build` can be used to execute the
-- underlying Docker build once and re-use the resulting `Image`.
build :: MonadDocker m => ToImage -> m ToImage
build toImage@ToImage { applyToContainerRequest } = do
  image <- runToImage toImage
  return $ ToImage
    {
      runToImage = pure image
    , applyToContainerRequest
    }


-- | Default `ToImage`. Doesn't apply anything to to `ContainerRequests`.
defaultToImage :: (forall m . MonadDocker m => m Image) -> ToImage
defaultToImage action = ToImage
  {
    runToImage = action
  , applyToContainerRequest = \x -> x
  }


-- | Get an `Image` from a tag.
fromTag :: ImageTag -> ToImage
fromTag imageTag = defaultToImage $ do
  output <- docker [ "pull", "--quiet", imageTag ]
  return $ Image
    {
      tag = strip (pack output)
    }


-- | Build the image from a build path and an optional path to the
-- Dockerfile (default is Dockerfile)
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
type ContainerId = Text


-- | Dumb logger abstraction to allow us to trace container execution.
data Logger = Logger
  {
    debug :: forall m . (HasCallStack, MonadIO m) => Text -> m ()
  , info  :: forall m . (HasCallStack, MonadIO m) => Text -> m ()
  , warn  :: forall m . (HasCallStack, MonadIO m) => Text -> m ()
  , error :: forall m . (HasCallStack, MonadIO m) => Text -> m ()
  }


-- | Logger that doesn't log anything.
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
newtype WaitUntilReady = WaitUntilReady
  {
    checkContainerReady :: Logger -> Container -> ResIO ()
  }


-- | The exception thrown by `waitForLine` in case the expected log line
-- wasn't found.
newtype UnexpectedEndOfPipe = UnexpectedEndOfPipe
  {
    -- | The id of the underlying container.
    id :: ContainerId
  }
  deriving (Eq, Show)


instance Exception UnexpectedEndOfPipe


-- | The exception thrown by `waitUntilTimeout`.
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
waitUntilTimeout :: Int -> WaitUntilReady -> WaitUntilReady
waitUntilTimeout seconds wait = WaitUntilReady $ \logger container@Container{ id } -> do
  withRunInIO $ \runInIO -> do
    result <- timeout (seconds * 1000000) $
      runInIO (checkContainerReady wait logger container)
    case result of
      Nothing ->
        throwM $ TimeoutException { id }
      Just _ ->
        pure ()


-- | Waits until the port of a container is ready to accept connections.
-- This combinator should always be used with `waitUntilTimeout`.
waitUntilMappedPortReachable
  :: Int
  -> WaitUntilReady
waitUntilMappedPortReachable port = WaitUntilReady $ \logger container ->
  withFrozenCallStack $ do

  let
    hostIp :: String
    hostIp = unpack (containerIp container)

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
waitWithLogs :: (Container -> Handle -> Handle -> IO ()) -> WaitUntilReady
waitWithLogs waiter = WaitUntilReady $ \_logger container -> do
  withLogs container $ \stdout stderr ->
    liftIO $ waiter container stdout stderr


-- | A data type indicating which pipe to scan for a specific log line.
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
waitUntilReady :: MonadDocker m => Container -> WaitUntilReady -> m ()
waitUntilReady container waiter =
  liftResourceT $ checkContainerReady waiter silentLogger container


-- | Handle to a Docker image.
data Image = Image
  {
    -- | The image tag assigned by Docker. Uniquely identifies an `Image`
    -- within Docker.
    tag :: ImageTag
  }
  deriving (Eq, Show)


-- | The image tag assigned by Docker. Uniquely identifies an `Image`
-- within Docker.
imageTag :: Image -> ImageTag
imageTag Image { tag } = tag


-- | Handle to a Docker container.
data Container = Container
  {
    -- | The container Id assigned by Docker, uniquely identifying this `Container`.
    id            :: ContainerId
    -- | Underlying `ReleaseKey` for the resource finalizer.
  , releaseKey    :: ReleaseKey
    -- | The underlying `Image` of this container.
  , image         :: Image
    -- | Memoized output of `docker inspect`. This is being calculated lazily.
  , inspectOutput :: InspectOutput
  }


-- | The parsed JSON output of docker inspect command.
type InspectOutput = Value


-- | Returns the id of the container.
containerId :: Container -> ContainerId
containerId Container { id } = id


-- | Returns the underlying image of the container.
containerImage :: Container -> Image
containerImage Container { image } = image


-- | Returns the internal release key used for safely shutting down
-- the container. Use this with care. This function is considered
-- an internal detail.
containerReleaseKey :: Container -> ReleaseKey
containerReleaseKey Container { releaseKey } = releaseKey


-- | Looks up the ip address of the container.
containerIp :: Container -> Text
containerIp Container { id, inspectOutput } =
  case inspectOutput
    ^? Lens.key "NetworkSettings"
    . Lens.key "IPAddress"
    . Lens._String of

    Nothing ->
      throw $ InspectOutputUnexpected { id }

    Just address ->
      address


-- | Looks up an exposed port on the host.
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
      ^? Lens.key "NetworkSettings"
      . Lens.key "Ports"
      . Lens.key textPort
      . Lens.values
      . Lens.key "HostPort"
      . Lens._String of

      Nothing ->
        throw $ UnknownPortMapping
          {
            id
          , port = textPort
          }
      Just hostPort ->
        read (unpack hostPort)


-- | Runs the `docker inspect` command. Memoizes the result.
inspect :: MonadDocker m => Container -> m InspectOutput
inspect Container { inspectOutput } =
  pure inspectOutput


-- | Runs the `docker inspect` command.
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
