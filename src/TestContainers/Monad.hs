{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module TestContainers.Monad
  ( MonadDocker,

    -- * Runtime configuration
    Config (..),
    defaultConfig,
    defaultDockerConfig,
    determineConfig,
  )
where

import Control.Monad.Catch
  ( MonadCatch,
    MonadMask,
    MonadThrow,
  )
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Resource (MonadResource)
import TestContainers.Trace (Tracer)

-- | Docker related functionality is parameterized over this `Monad`.
--
-- @since 0.1.0.0
type MonadDocker m =
  ( MonadIO m,
    MonadMask m,
    MonadThrow m,
    MonadCatch m,
    MonadResource m,
    MonadReader Config m
  )

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
