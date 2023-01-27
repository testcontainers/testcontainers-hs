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
--
-- containers :: MonadDocker m => m Boolean
-- containers = do
--   _redis <- TestContainers.run $ TestContainers.containerRequest TestContainers.redis
--   _kafka <- TestContainers.run $ TestContainers.containerRequest TestContainers.kafka
--   pure True
--
-- example :: Spec
-- example =
--   around (withContainers containers) $ describe "Example tests"
--     it "first test" $ \\isBoolean -> do
--       isBoolean `shouldBe` True
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
