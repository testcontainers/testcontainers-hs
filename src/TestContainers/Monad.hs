{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module TestContainers.Monad
  ( -- * Monad
    MonadDocker,
    TestContainer,
    runTestContainer,

    -- * Runtime configuration
    Config (..),
  )
where

import Control.Applicative (liftA2)
import Control.Monad.Catch
  ( MonadCatch,
    MonadMask,
    MonadThrow,
  )
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT, runReaderT)
import Control.Monad.Trans.Resource (MonadResource, ResourceT, runResourceT)
import Data.IORef (newIORef, readIORef, writeIORef)
import TestContainers.Docker.Reaper (Reaper)
import TestContainers.Trace (Tracer)

newtype TestContainerEnv = TestContainerEnv
  { config :: Config
  }

-- | The heart and soul of the testcontainers library.
--
-- @since 0.4.0.0
newtype TestContainer a = TestContainer {unTestContainer :: ReaderT TestContainerEnv (ResourceT IO) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadMask,
      MonadCatch,
      MonadThrow,
      MonadResource,
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
-- @since 0.4.0.0
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

  runResourceT
    ( runReaderT
        (unTestContainer action)
        ( TestContainerEnv
            { config =
                config
                  { configCreateReaper = getOrCreateReaper
                  }
            }
        )
    )

-- | Docker related functionality is parameterized over this `Monad`. Since 0.4.0.0 this is
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
    configCreateReaper :: TestContainer Reaper
  }
