{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module TestContainers.Docker.Internal
  ( DockerException (..),

    -- * Container related stuff
    ContainerId,
    InspectOutput,

    -- * Network related stuff
    NetworkId,

    -- * Running docker
    docker,
    dockerWithStdin,

    -- * Following logs
    Pipe (..),
    LogConsumer,
    consoleLogConsumer,
    prefixedLogConsumer,
    dockerFollowLogs,

    -- * Common abstractions for Docker resources
    WithoutReaper (..),
  )
where

import qualified Control.Concurrent.Async as Async
import Control.Exception (Exception)
import Control.Monad (forever)
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (MonadResource, allocate)
import Data.Aeson (Value)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Foldable (traverse_)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import System.Exit (ExitCode (..))
import qualified System.IO
import qualified System.Process as Process
import TestContainers.Trace (Trace (..), Tracer, withTrace)

-- | Shared property between Docker resources.
class WithoutReaper request where
  -- | Do not register the docker resource (container, register, etc.) with the resource reaper.
  -- Careful, doing this will make your container leak on shutdown if not explicitly stopped.
  --
  -- @since 0.5.1.0
  withoutReaper :: request -> request

-- | Identifies a network within the Docker runtime. Assigned by @docker network create@
--
-- @since 0.5.0.0
type NetworkId = Text

-- | Identifies a container within the Docker runtime. Assigned by @docker run@.
--
-- @since 0.1.0.0
type ContainerId = Text

-- | The parsed JSON output of docker inspect command.
--
-- @since 0.1.0.0
type InspectOutput = Value

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
        port :: Text,
        -- | The image tag of the container
        imageName :: Maybe Text,
        -- | The container name
        containerName :: Maybe Text
      }
  deriving (Eq, Show)

instance Exception DockerException

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

-- | A data type indicating which pipe to scan for a specific log line.
--
-- @since 0.1.0.0
data Pipe
  = -- | Refer to logs on STDOUT.
    Stdout
  | -- | Refer to logs on STDERR.
    Stderr
  deriving stock (Eq, Ord, Show)

-- | An abstraction for forwarding logs.
--
-- @since 0.5.0.0
type LogConsumer = Pipe -> ByteString -> IO ()

-- | A simple 'LogConsumer' that writes log lines to stdout and stderr respectively.
--
-- @since 0.5.0.0
consoleLogConsumer :: LogConsumer
consoleLogConsumer pipe line = do
  case pipe of
    Stdout -> do
      ByteString.hPutStr System.IO.stdout line
      ByteString.hPut System.IO.stdout (ByteString.singleton 0x0a)
    Stderr -> do
      ByteString.hPutStr System.IO.stderr line
      ByteString.hPut System.IO.stderr (ByteString.singleton 0x0a)

-- | A 'LogConsumer' that prefixes with some prefix identifier.
-- This makes it easier to identify which containers/callsites produced which logs.
prefixedLogConsumer :: Text -> LogConsumer
prefixedLogConsumer prefix pipe line = do
  let prefixText = "[" <> prefix <> "] "
      prefixedLine = encodeUtf8 prefixText <> line
  case pipe of
    Stdout -> do
      ByteString.hPutStr System.IO.stdout prefixedLine
      ByteString.hPut System.IO.stdout (ByteString.singleton 0x0a)
    Stderr -> do
      ByteString.hPutStr System.IO.stderr prefixedLine
      ByteString.hPut System.IO.stderr (ByteString.singleton 0x0a)

-- | Forwards container logs to a 'LogConsumer'. This is equivalent of calling @docker logs containerId --follow@
--
-- @since 0.5.0.0
dockerFollowLogs :: (MonadResource m) => Tracer -> ContainerId -> LogConsumer -> m ()
dockerFollowLogs tracer containerId logConsumer = do
  let dockerArgs =
        ["logs", containerId, "--follow"]

  (_releaseKey, _result) <-
    allocate
      ( do
          process@(_stdin, Just stdout, Just stderr, _processHandle) <-
            Process.createProcess $
              (Process.proc "docker" (map unpack dockerArgs))
                { Process.std_out = Process.CreatePipe,
                  Process.std_err = Process.CreatePipe
                }

          withTrace tracer (TraceDockerFollowLogs dockerArgs)

          stdoutReporter <- Async.async $ do
            forever $ do
              line <- ByteString.hGetLine stdout
              logConsumer Stdout line

          stderrReporter <- Async.async $ do
            forever $ do
              line <- ByteString.hGetLine stderr
              logConsumer Stderr line

          pure (process, stdoutReporter, stderrReporter)
      )
      ( \(process, stdoutReporter, stderrReporter) -> do
          Async.cancel stdoutReporter
          Async.cancel stderrReporter
          Process.cleanupProcess process
      )

  pure ()
