{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TestContainers.Trace
  ( -- * TestContainer traces
    Trace (..),

    -- * Tracer
    Tracer,
    newTracer,
    withTrace,
  )
where

import Control.Exception (IOException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import System.Exit (ExitCode)

-- | Type representing various events during testcontainer execution.
data Trace
  = -- | The low-level invocation of @docker@ command
    --
    -- @
    --   TraceDockerInvocation args stdin exitcode
    -- @
    TraceDockerInvocation [Text] Text ExitCode -- docker [args] [stdin]
  | -- | Line written to STDOUT by a Docker process.
    TraceDockerStdout Text
  | -- | Line written to STDERR by a Docker process.
    TraceDockerStderr Text
  | -- | Waiting for a container to become ready. Attached with the
    -- timeout to wait (in seconds).
    TraceWaitUntilReady (Maybe Int)
  | -- | Opening socket
    TraceOpenSocket Text Int (Maybe IOException)
  | -- | Call HTTP endpoint
    TraceHttpCall Text Int (Either String Int)
  deriving stock (Eq, Show)

-- | Traces execution within testcontainers library.
newtype Tracer = Tracer {unTracer :: Trace -> IO ()}
  deriving newtype (Semigroup, Monoid)

-- | Construct a new `Tracer` from a tracing function.
newTracer ::
  (Trace -> IO ()) ->
  Tracer
newTracer action =
  Tracer
    { unTracer = action
    }

withTrace :: (MonadIO m) => Tracer -> Trace -> m ()
withTrace tracer trace =
  liftIO $ unTracer tracer trace
{-# INLINE withTrace #-}
