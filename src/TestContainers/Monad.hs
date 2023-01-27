{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module TestContainers.Monad
  ( -- * Monad
    MonadDocker,
    TestContainer,
    runTestContainer,

    -- * Runtime configuration
    Config (..),
    defaultConfig,
    defaultDockerConfig,
    determineConfig,
  )
where

import Control.Applicative (liftA2)
import Control.Monad.Catch
  ( MonadCatch,
    MonadMask,
    MonadThrow,
  )
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.Trans.Resource (MonadResource, MonadUnliftIO, ResourceT, runResourceT)
import TestContainers.Trace (Tracer)

-- | The heart and soul of the testcontainers library.
--
-- @since x.x.x
newtype TestContainer a = TestContainer {unTestContainer :: ReaderT Config (ResourceT IO) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadUnliftIO,
      MonadMask,
      MonadCatch,
      MonadThrow,
      MonadResource,
      MonadReader Config
    )

instance (Semigroup a) => Semigroup (TestContainer a) where
  (<>) =
    liftA2 (<>)

instance (Monoid a) => Monoid (TestContainer a) where
  mempty = pure mempty

-- | Run a 'TestContainer' action. Any container spun up during the computation are guaranteed
-- to be shutdown and cleaned up once this function returns.
--
-- @since x.x.x
runTestContainer :: Config -> TestContainer a -> IO a
runTestContainer config action =
  runResourceT (runReaderT (unTestContainer action) config)

-- | Docker related functionality is parameterized over this `Monad`. Since x.x.x this is
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
    configTracer :: Tracer
  }

-- | Default configuration.
--
-- @since x.x.x
defaultConfig :: Config
defaultConfig =
  Config
    { configDefaultWaitTimeout = Just 60,
      configTracer = mempty
    }

-- | Default configuration.
--
-- @since 0.2.0.0
defaultDockerConfig :: Config
defaultDockerConfig =
  defaultConfig

-- | Autoselect the default configuration depending on wether you use Docker For
-- Mac or not.
determineConfig :: IO Config
determineConfig =
  pure defaultDockerConfig
