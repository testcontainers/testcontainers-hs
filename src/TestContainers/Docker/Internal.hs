{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module TestContainers.Docker.Internal
  ( DockerException (..),

    -- * Container related stuff
    ContainerId,

    -- * Network related stuff
    NetworkId,

    -- * Running docker
    docker,
    dockerWithStdin,
  )
where

import Control.Exception (Exception)
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (traverse_)
import Data.Text (Text, pack, unpack)
import System.Exit (ExitCode (..))
import qualified System.Process as Process
import TestContainers.Trace (Trace (..), Tracer, withTrace)

-- | Identifies a network within the Docker runtime. Assigned by @docker network create@
--
-- @since x.x.x
type NetworkId = Text

-- | Identifies a container within the Docker runtime. Assigned by @docker run@.
--
-- @since 0.1.0.0
type ContainerId = Text

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
