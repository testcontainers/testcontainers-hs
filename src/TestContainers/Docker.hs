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
  , Image(Image, tag)

  -- * Docker container

  , ContainerId
  , Container(Container, id, releaseKey, image)

  -- * Referring to images

  , ToImage

  , fromTag
  , fromBuildContext
  , fromTarballContext
  , fromDockerfile

  , build

  -- * Running containers

  , ContainerRequest
  , defaultContainerRequest
  , setName
  , setCmd
  , setRm
  , setEnv
  , setLink
  , setExpose
  , setPublish
  , run

  -- * Managing the container lifecycle

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

  -- * Reexports for convenience
  , ResIO
  , runResourceT
  ) where

import           Control.Monad.Catch          (Exception, MonadCatch, MonadMask,
                                               MonadThrow, bracket, throwM)
import           Control.Monad.IO.Class       (MonadIO (liftIO))
import           Control.Monad.IO.Unlift      (MonadUnliftIO (withRunInIO))
import           Control.Monad.Trans.Resource (MonadResource (liftResourceT),
                                               ReleaseKey, ResIO, register,
                                               runResourceT)
import qualified Data.ByteString.Lazy.Char8   as LazyByteString
import           Data.List                    (find)
import           Data.Text                    (Text, pack, strip, unpack)
import           Data.Text.Encoding.Error     (lenientDecode)
import qualified Data.Text.Lazy               as LazyText
import qualified Data.Text.Lazy.Encoding      as LazyText
import           Prelude                      hiding (id)
import           System.IO                    (Handle, hClose)
import qualified System.Process               as Process
import           System.Timeout               (timeout)


-- | Docker related functionality is parameterized over this `Monad`.
type MonadDocker m =
  (MonadIO m, MonadMask m, MonadThrow m, MonadCatch m, MonadResource m)


-- | Parameters for a running a Docker container.
data ContainerRequest = ContainerRequest
  {
    cmd          :: Maybe [Text]
  , env          :: [(Text, Text)]
  , exposedPorts :: [Int]
  , publishPorts :: [Text]
  , volumeMounts :: [(Text, Text)]
  , links        :: [ContainerId]
  , name         :: Maybe Text
  , rmOnExit     :: Bool
  }


-- | Default `ContainerRequest`. Used as base for every Docker container.
defaultContainerRequest :: ContainerRequest
defaultContainerRequest = ContainerRequest
  {
    name         = Nothing
  , cmd          = Nothing
  , env          = []
  , exposedPorts = []
  , publishPorts = []
  , volumeMounts = []
  , links        = []
  , rmOnExit     = True
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
setExpose :: [Int] -> ContainerRequest -> ContainerRequest
setExpose newExpose req =
  req { exposedPorts = newExpose }


-- | Set published ports on the container.
setPublish :: [Text] -> ContainerRequest -> ContainerRequest
setPublish newPublish req =
  req { publishPorts = newPublish }


-- | Runs a Docker container from an `Image` and `ContainerRequest`. A finalizer
-- is registered so that the container is aways stopped when it goes out of scope.
run :: MonadDocker m => ToImage -> ContainerRequest -> m Container
run toImage containerRequest = do
  image@Image{ tag } <- runToImage toImage

  let
    ContainerRequest
      {
        name
      , cmd
      , env
      , exposedPorts
      , publishPorts
      , volumeMounts
      , links
      , rmOnExit
      } = containerRequest

    dockerRun :: [Text]
    dockerRun = concat $
      [ [ "run" ] ] ++
      [ [ "--detach" ] ] ++
      [ [ "--name", containerName ] | Just containerName <- [name] ] ++
      [ [ "--env", variable <> "=" <> value  ] | (variable, value) <- env ] ++
      [ [ "--expose", pack (show port)] | port <- exposedPorts ] ++
      [ [ "--publish", port ] | port <- publishPorts ] ++
      [ [ "--link", container ] | container <- links ] ++
      [ [ "--volume", src <> ":" <> dest ] | (src, dest) <- volumeMounts ] ++
      [ [ "--rm" ] | rmOnExit ] ++
      [ [ tag ] ] ++
      [ command | Just command <- [cmd] ]

  (_exitCode, stdout, _stderr) <- liftIO $ Process.readProcessWithExitCode
    "docker"
    (map unpack dockerRun)
    ""

  let
    containerId :: ContainerId
    !containerId =
      strip (pack stdout)

  releaseKey <- register (stop' containerId)

  pure $ Container
    {
      id = strip (pack stdout)
    , releaseKey
    , image
    }


-- | Kills a Docker container.
kill :: MonadDocker m => Container -> m ()
kill Container { id } = do
  (_exitCode, _stdout, _stderr) <- liftIO $ Process.readProcessWithExitCode
    "docker"
    [ "kill", unpack id ]
    ""
  return ()


-- | Stops a Docker container.
stop :: MonadDocker m => Container -> m ()
stop Container { id } = do
  stop' id


-- | Stops a Docker container. Referring to the container only by ID. This is
-- considered internal.
stop' :: MonadIO m => ContainerId -> m ()
stop' containerId = do
  (_exitCode, _stdout, _stderr) <- liftIO $ Process.readProcessWithExitCode
    "docker"
    [ "stop", unpack containerId ]
    ""
  return ()


-- | Remove a Docker container.
rm :: MonadDocker m => Container -> m ()
rm Container { id } = do
  (_exitCode, _stdout, _stderr) <- liftIO $ Process.readProcessWithExitCode
    "docker"
    [ "rm", "-f", "-v", unpack id ]
    ""
  return ()


-- | Get the logs from a Docker container.
withLogs :: forall m a . MonadDocker m => Container -> (Handle -> Handle -> m a) -> m a
withLogs Container { id } logger = do

  let
    acquire :: m (Handle, Handle, Handle, Process.ProcessHandle)
    acquire =
      liftIO $ Process.runInteractiveProcess
        "docker"
        [ "logs", unpack id ]
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
  (_exitCode, stdout, _stderr) <- liftIO $ Process.readProcessWithExitCode
    "docker"
    [ "pull", "--quiet", unpack imageTag ]
    ""
  return $ Image
    {
      tag = strip (pack stdout)
    }


-- | Build the image from a build path and an optional path to the
-- Dockerfile (default is Dockerfile)
fromBuildContext
  :: FilePath
  -> Maybe FilePath
  -> ToImage
fromBuildContext =
  undefined


-- | Build an image from a tar stream. Useful if you want to create images dynamically
-- from code.
fromTarballContext
  :: LazyByteString.ByteString
  -> (Maybe FilePath)
  -> ToImage
fromTarballContext =
  undefined


-- | Build a contextless image only from a Dockerfile passed as `Text`.
fromDockerfile
  :: Text
  -> ToImage
fromDockerfile =
  undefined


-- | Identifies a container within the Docker runtime. Assigned by @docker run@.
type ContainerId = Text


-- | A strategy that describes how to asses readiness of a `Container`. Allows
-- Users to plug in their definition of readiness.
newtype WaitUntilReady = WaitUntilReady
  {
    checkContainerReady :: Container -> ResIO ()
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


-- | @waitUntilTimeout n waitUntilReady@ waits @n@ microseconds for the container
-- to be ready. If the container is not ready by then a `TimeoutException` will
-- be thrown.
waitUntilTimeout :: Int -> WaitUntilReady -> WaitUntilReady
waitUntilTimeout microseconds wait = WaitUntilReady $ \container@Container{ id } -> do
  withRunInIO $ \runInIO -> do
    result <- timeout microseconds $ runInIO (checkContainerReady wait container)
    case result of
      Nothing ->
        throwM $ TimeoutException { id }
      Just _ ->
        pure ()


-- | A low-level primitive that allows scanning the logs for specific log lines
-- that indicate readiness of a container.
--
-- The `Handle`s passed to the function argument represent @stdout@ and @stderr@
-- of the container.
waitWithLogs :: (Container -> Handle -> Handle -> IO ()) -> WaitUntilReady
waitWithLogs waiter = WaitUntilReady $ \container -> do
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


-- | Handle to a Docker image.
data Image = Image
  {
    -- | The image tag assigned by Docker. Uniquely identifies an `Image`
    -- within Docker.
    tag :: ImageTag
  }
  deriving (Eq, Show)


-- | Handle to a Docker container.
data Container = Container
  {
    -- | The container Id assigned by Docker, uniquely identifying this `Container`.
    id         :: ContainerId
    -- | Underlying `ReleaseKey` for the resource finalizer.
  , releaseKey :: ReleaseKey
    -- | The underlying `Image` of this container.
  , image      :: Image
  }


-- | Blocks until the container is ready. `waitUntilReady` might throw exceptions
-- depending on the used `WaitUntilReady` on the container.
waitUntilReady :: MonadDocker m => Container -> WaitUntilReady -> m ()
waitUntilReady container waiter =
  liftResourceT $ checkContainerReady waiter container
