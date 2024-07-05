{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestContainers.Hspec
  ( -- * Running containers for tests
    withContainers,

    -- * Re-exports for convenience
    module Reexports,
  )
where

import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource
  ( InternalState,
    getInternalState,
    liftResourceT,
  )
import Control.Monad.Trans.Resource.Internal
  ( stateAlloc,
    stateCleanup,
  )
import Data.Acquire (ReleaseType (ReleaseNormal))
import TestContainers as Reexports
import TestContainers.Monad (runTestContainer)

-- | Allow `Hspec.Spec` to depend on Docker containers. Hspec takes care of
-- initialization and de-initialization of the containers.
--
-- @
-- data ContainerPorts = ContainerPorts {
--   redisPort :: Int,
--   kafkaPort :: Int
-- }
--
-- containers :: MonadDocker m => m ContainerPorts
-- containers = do
--   redis <- TestContainers.run $ TestContainers.containerRequest TestContainers.redis
--   kafka <- TestContainers.run $ TestContainers.containerRequest TestContainers.kafka
--   pure ContainerPorts {
--     redisPort = TestContainers.containerPort redis "6379/tcp",
--     kafkaPort = TestContainers.containerPort kafka "9092/tcp"
--   }
--
-- example :: Spec
-- example =
--   around (withContainers containers) $ describe "Example tests"
--     it "some test that uses redis and kafka" $ \ContainerPorts{redisPort, kafkaPort} -> do
--       redisPort `shouldNotBe` kafkaPort
-- @
--
-- `withContainers` allows you naturally scope the handling of containers for your tests.
withContainers ::
  forall a.
  TestContainer a ->
  (a -> IO ()) ->
  IO ()
withContainers startContainers = dropState $ bracket acquire release
  where
    runC action = do
      config <- determineConfig
      runTestContainer config action

    acquire :: IO (a, InternalState)
    acquire = runC $ do
      result <- startContainers
      releaseMap <- liftResourceT getInternalState

      liftIO $ stateAlloc releaseMap
      pure (result, releaseMap)

    release :: (a, InternalState) -> IO ()
    release (_, internalState) =
      stateCleanup ReleaseNormal internalState

    dropState :: (((a, b) -> c) -> c) -> (a -> c) -> c
    dropState = (. (. fst))
