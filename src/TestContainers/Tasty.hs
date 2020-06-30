{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TestContainers.Tasty
  (
    -- * Running containers for tests
    withContainers

    -- * Re-exports for convenience
  , module Reexports
  ) where

import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad.Trans.Resource          (InternalState,
                                                        getInternalState,
                                                        runResourceT)
import           Control.Monad.Trans.Resource.Internal (stateAlloc,
                                                        stateCleanup)
import           Data.Acquire                          (ReleaseType (ReleaseNormal))
import           Test.Tasty                            (TestTree, withResource)

import           TestContainers                        (Container,
                                                        ContainerRequest,
                                                        MonadDocker, ToImage,
                                                        run, stop)
import           TestContainers                        as Reexports


-- | Allow `Tasty.TestTree` to depend on Docker containers. Tasty takes care of
-- initialization and de-initialization of the containers.
--
-- @
--
-- containers :: MonadDocker m => m ()
-- containers = do
--   _redis <- TestContainers.run TestContainers.redis TestContainers.defaultContainerRequest
--   _kafka <- TestContainers.run TestContainers.kafka TestContainers.defaultContainerRequest
--   pure ()
--
-- example :: TestTree
-- example =
--   withContainers containers $ \\runContainers -> testGroup "Example tests"
--     [
--       testCase "first test" $ do
--         -- Actually runs the container.
--         runContainers
--       testCase "second test" $ do
--         -- Start containers. Tasty makes sure to only initialize once as
--         --  `first test` might already have started them.
--         runContainers
--     ]
-- @
--
-- `withContainers` allows you naturally scope the handling of containers for your tests.
withContainers
  :: forall a
  .  (forall m. MonadDocker m => m a)
  -> (IO a -> TestTree)
  -> TestTree
withContainers startContainers tests =
  let
    -- Correct resource handling is tricky here:
    -- Tasty offers a bracket alike in IO. We  have
    -- to transfer the ReleaseMap of the ResIO safely
    -- to the release function. Fortunately resourcet
    -- let's us access the internal state..
    acquire :: IO (a, InternalState)
    acquire = runResourceT $ do
      result     <- startContainers
      releaseMap <- getInternalState

      -- N.B. runResourceT runs the finalizers on every
      -- resource. We don't want it to! We want to run
      -- finalization in the release function that is
      -- called by Tasty! stateAlloc increments a references
      -- count to accomodate for exactly these kind of
      -- cases.
      liftIO $ stateAlloc releaseMap
      pure (result, releaseMap)

    release :: (a, InternalState) -> IO ()
    release (_, internalState) =
      stateCleanup ReleaseNormal internalState
  in
    withResource acquire release $ \mk ->
      tests (fmap fst mk)
