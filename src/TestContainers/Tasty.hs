{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TestContainers.Tasty
  (
    -- * Tasty Ingredient
    ingredient

    -- * Running containers for tests
  , withContainers

    -- * Re-exports for convenience
  , module Reexports
  ) where

import           Control.Applicative                   ((<|>))
import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad.Reader                  (runReaderT)
import           Control.Monad.Trans.Resource          (InternalState,
                                                        getInternalState)
import           Control.Monad.Trans.Resource.Internal (stateAlloc,
                                                        stateCleanup)
import           Data.Acquire                          (ReleaseType (ReleaseNormal))
import           Test.Tasty                            (TestTree, askOption,
                                                        withResource)
import qualified Test.Tasty                            as Tasty
import           Test.Tasty.Ingredients                (Ingredient)
import           Test.Tasty.Options                    (IsOption (..),
                                                        OptionDescription (..),
                                                        safeRead)

import           Data.Data                             (Proxy (Proxy))
import           TestContainers                        as Reexports


newtype DefaultTimeout = DefaultTimeout (Maybe Int)


instance IsOption DefaultTimeout where

  defaultValue =
    DefaultTimeout Nothing

  parseValue =
    fmap (DefaultTimeout . Just) . safeRead

  optionName =
    pure "testcontainers-default-timeout"

  optionHelp =
    pure "The max. number of seconds to wait for a container to become ready"


-- | Tasty `Ingredient` that adds useful options to control defaults within the
-- TetContainers library.
--
-- @
-- main :: IO ()
-- main = `Tasty.defaultMainWithIngredients` (`ingredient` : `Tasty.defaultIngredients`) tests
-- @
--
-- @since 0.3.0.0
--
ingredient :: Ingredient
ingredient = Tasty.includingOptions
  [
    Option (Proxy :: Proxy DefaultTimeout)
  ]


withContainers
  :: forall a
  .  (forall m. MonadDocker m => m a)
  -> (IO a -> TestTree)
  -> TestTree
withContainers startContainers tests =
  askOption $ \ (DefaultTimeout defaultTimeout) ->

  let
    runC action = do
      config <- determineConfig

      let
        actualConfig = config
          {
            configDefaultWaitTimeout =
              defaultTimeout <|> configDefaultWaitTimeout config
          }

      runReaderT (runResourceT action) actualConfig

    -- Correct resource handling is tricky here:
    -- Tasty offers a bracket alike in IO. We  have
    -- to transfer the ReleaseMap of the ResIO safely
    -- to the release function. Fortunately resourcet
    -- let's us access the internal state..
    acquire :: IO (a, InternalState)
    acquire = runC $ do
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
