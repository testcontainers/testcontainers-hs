{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module TestContainers.Monad
  ( -- * Configuration
    Config (..),
    defaultDockerConfig,
    determineConfig,

    -- * Monad
    MonadDocker,
    TestContainer,
    runTestContainer,
  )
where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.Trans.Resource (MonadResource, ResourceT, runResourceT)
import TestContainers.Tracer (Tracer)

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

-- \| Default configuration.
--
-- @since 0.2.0.0
defaultDockerConfig :: Config
defaultDockerConfig =
  Config
    { configDefaultWaitTimeout = Just 60,
      configTracer = mempty
    }

-- | Autoselect the default configuration depending on wether you use Docker For
-- Mac or not.
determineConfig :: IO Config
determineConfig =
  pure defaultDockerConfig

-- | Docker related functionality is parameterized over this `Monad`.
--
-- @since 0.1.0.0
type MonadDocker m = (m ~ TestContainer)

{-# DEPRECATED MonadDocker "Use 'TestContainer' monad instead" #-}

-- | Concrete monad that is used to orchestrate containers.
-- 
-- @since x.x.x.x
newtype TestContainer a = TestContainer {unTestContainer :: ReaderT Config (ResourceT IO) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadMask,
      MonadThrow,
      MonadCatch,
      MonadResource,
      MonadReader Config,
      MonadUnliftIO
    )

runTestContainer :: Config -> TestContainer a -> IO a
runTestContainer config action =
  runResourceT (runReaderT (unTestContainer action) config)
