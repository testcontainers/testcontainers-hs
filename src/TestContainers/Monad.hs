{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module TestContainers.Monad
  ( -- * Monad
    MonadDocker,
    TestContainer,
    runTestContainer,

    -- * Blocking during concurrent execution
    defer,

    -- * Runtime configuration
    RunStrategy (..),
    Config (..),
  )
where

import Control.Applicative (liftA2)
import qualified Control.Concurrent.Async
import Control.Exception (evaluate)
import Control.Monad (sequence_)
import Control.Monad.Catch
  ( MonadCatch,
    MonadMask,
    MonadThrow,
  )
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO (..), askRunInIO)
import Control.Monad.Reader (MonadReader (..), ReaderT, runReaderT)
import qualified Control.Monad.Trans.Resource
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.IORef
import qualified System.IO.Unsafe
import TestContainers.Docker.Reaper (Reaper)
import TestContainers.Trace (Tracer)

data RunStrategy
  = -- | Run containers sequentially. This is the default behaviour of the runtime.
    --
    -- @since x.x.x
    SequentialRunStrategy
  | -- | Run and resolve graph of containers concurrently. This requires explicit
    -- dependency annotations across the container graph.
    --
    -- An optional limit on concurrent @docker run@ invocations can be provided.
    --
    -- @since x.x.x
    ConcurrentRunStrategy (Maybe Int)

data TestContainerEnv = TestContainerEnv
  { config :: Config,
    -- | In the concurrent run strategy we need a way to block execution until every
    -- container is done running. This barrier is used at the end of the execution and
    -- blocks for them to finish.
    barrierRef :: Data.IORef.IORef [IO ()]
  }

-- | The heart and soul of the testcontainers library.
--
-- @since 0.5.0.0
newtype TestContainer a = TestContainer
  { unTestContainer :: ReaderT TestContainerEnv (Control.Monad.Trans.Resource.ResourceT IO) a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadMask,
      MonadCatch,
      MonadThrow,
      Control.Monad.Trans.Resource.MonadResource,
      MonadFix
    )

-- Instance defined without newtype deriving as GHC has a hard time
-- deriving it for old versions of unliftio.
instance MonadUnliftIO TestContainer where
  withRunInIO action = TestContainer $
    withRunInIO $ \runInIo ->
      action (runInIo . unTestContainer)

instance MonadReader Config TestContainer where
  ask = TestContainer $ do
    TestContainerEnv {config} <- ask
    pure config

  local f (TestContainer action) = TestContainer $ do
    local (\env@TestContainerEnv {config} -> env {config = f config}) action

instance (Semigroup a) => Semigroup (TestContainer a) where
  (<>) =
    liftA2 (<>)

instance (Monoid a) => Monoid (TestContainer a) where
  mempty = pure mempty

-- | Run a 'TestContainer' action. Any container spun up during the computation are guaranteed
-- to be shutdown and cleaned up once this function returns.
--
-- @since 0.5.0.0
runTestContainer :: Config -> TestContainer a -> IO a
runTestContainer config action = do
  -- Ensure through caching that there is only ever exactly
  -- one 'Reaper' per session.
  reaperRef <- newIORef Nothing
  let getOrCreateReaper = do
        mreaper <- liftIO (readIORef reaperRef)
        case mreaper of
          Just reaper ->
            pure reaper
          Nothing -> do
            reaper <- configCreateReaper config
            liftIO (writeIORef reaperRef (Just reaper))
            pure reaper

  barrierRef <- newIORef []

  Control.Monad.Trans.Resource.runResourceT $ do
    result <-
      runReaderT
        (unTestContainer action)
        ( TestContainerEnv
            { barrierRef,
              config =
                config
                  { configCreateReaper = getOrCreateReaper
                  }
            }
        )

    -- Take the barrier and wait for execution
    liftIO $ do
      barrier <- readIORef barrierRef
      sequence_ barrier

    pure result

defer :: IO () -> TestContainer ()
defer action = TestContainer $ do
  TestContainerEnv {barrierRef} <- ask
  liftIO $ do
    barrier <- readIORef barrierRef
    writeIORef barrierRef $! action : barrier

-- | Docker related functionality is parameterized over this `Monad`. Since 0.5.0.0 this is
-- just a type alias for @m ~ 'TestContainer'@.
--
-- @since 0.1.0.0
type MonadDocker m =
  (m ~ TestContainer)

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
    configTracer :: Tracer,
    -- | How to obtain a 'Reaper'
    configCreateReaper :: TestContainer Reaper,
    -- |
    --
    -- @since x.x.x
    configRunStrategy :: RunStrategy
  }
