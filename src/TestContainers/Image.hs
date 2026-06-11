{-# LANGUAGE OverloadedStrings #-}

module TestContainers.Image
  ( -- * Collection of pre-defined Docker images
    redis,
    mongo,

    -- * Building and managing images
    module Docker,
  )
where

import TestContainers.Docker as Docker
  ( Image,
    ToImage,
    build,
    fromBuildContext,
    fromDockerfile,
    fromImageId,
    fromTag,
  )

-- | Image for Redis database.
--
-- @
-- redis = fromTag "redis:7.4"
-- @
--
-- @since 0.1.0.0
redis :: ToImage
redis =
  fromTag "redis:7.4"

-- | Image for Mongo database.
--
-- @
-- mongo = Tag "mongo:7.0"
-- @
--
-- @since 0.1.0.0
mongo :: ToImage
mongo =
  fromTag "mongo:7.0"
