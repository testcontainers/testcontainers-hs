# About

Testcontainers is a Haskell library that provides a friendly API to run Docker
containers. It is designed to create a runtime environment to use during your
integration tests

# Example

``` haskell
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import qualified Test.Tasty           as Tasty
import qualified Test.Tasty.HUnit     as Tasty
import qualified TestContainers.Tasty as TC


data Endpoints = Endpoints
  {
    redisHost :: String
  , redisPort :: Int
  }


-- | Sets up and runs the containers required for this test suite.
setupContainers :: TC.MonadDocker m => m Endpoints
setupContainers = do

  -- Launch the container based on the redis:5.0.0 image.
  redisContainer <- TC.run $ TC.containerRequest (TC.fromTag "redis:5.0.0")
    -- Expose the port 6379 from within the container. The respective port
    -- on the host machine can be looked up using `containerPort` (see below).
    TC.& TC.setExpose [ 6379 ]
    -- Wait until the container is ready to accept requests. `run` blocks until
    -- readiness can be established.
    TC.& TC.setWaitingFor (TC.waitUntilMappedPortReachable 6379)

  pure $ Endpoints
    {
      redisHost = "0.0.0.0"
    , redisPort =
        -- Look up the corresponding port on the host machine for the exposed
        -- port 6379.
        TC.containerPort redisContainer 6379
    }


main :: IO ()
main =
  Tasty.defaultMain $
  -- Use `withContainers` to make the containers available in the closed over
  -- tests. Due to how Tasty handles resources `withContainers` passes down
  -- an IO action `start` to actually start up the containers. `start` can be
  -- invoked multiple times, Tasty makes sure to only start up the containrs
  -- once.
  --
  -- `withContainers` ensures the started containers are shut down correctly
  -- once execution leaves its scope.
  TC.withContainers setupContainers $ \start ->
    Tasty.testGroup "Some test group"
      [
        Tasty.testCase "Redis test" $ do
          -- Actually start the containers!!
          Endpoints {..} <- start
          -- ... assert some properties
          pure ()

      , Tasty.testCase "Another Redis test" $ do
          -- Invoking `start` twice gives the same Endpoints!
          Endpoints {..} <- start
          -- ... assert some properties
          pure ()
      ]
```
