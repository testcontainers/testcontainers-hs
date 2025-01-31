module TestContainers.Config
  ( RunStrategy (..),
    Config (..),
    defaultConfig,
    defaultDockerConfig,
    determineConfig,
  )
where

import {-# SOURCE #-} TestContainers.Docker (createRyukReaper)
import TestContainers.Monad (Config (..), RunStrategy (..))

-- | Default configuration.
--
-- @since 0.5.0.0
defaultConfig :: Config
defaultConfig =
  Config
    { configDefaultWaitTimeout = Just 60,
      configTracer = mempty,
      configCreateReaper = createRyukReaper,
      configRunStrategy = SequentialRunStrategy
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
