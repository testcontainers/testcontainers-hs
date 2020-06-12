Testcontainers is a Haskell library that provides a friendly API to run Docker
container. It is designed to create runtime environment to use during your automatic
tests.

``` haskell
module Main where

import           Test.Tasty
import           Test.Tasty.HUnit
import           TestContainer.Tasty (MonadDocker, defaultContainerRequest,
                                      redis, run, withContainers)

containers :: MonadDocker m => m ()
containers = do
  _ <- run redis defaultContainerRequest
  pure ()


main :: IO ()
main = defaultMain $ testGroup "TestContainer tests"
  [
    withContainers containers $ \setup ->
      testCase "test1" $ do
        -- Start up the containers defined by `containers`. The library takes care
        -- of stopping and removing the containers once the test has completed.
        setup
        return ()
  ]
```
