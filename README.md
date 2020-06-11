# Example

``` haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.Trans.Resource (ResIO, allocate, runResourceT)
import           Data.Function                ((&))
import           Data.Text.Lazy               (isInfixOf)
import qualified TestContainer                as TC

main :: IO ()
main = do

  -- Spin up the redis container
  redis <- TC.run TC.redis $ TC.defaultContainerRequest
    & TC.setExpose [6379]
    & TC.setName "redis"

  TC.waitUntilReady redis $ TC.waitForLogLine TC.Stdout
    ("Ready to accept connections" `isInfixOf`)

  -- connect to redis and have some fun...

  TC.kill redis


-- Similar to main but with auto resource cleanup in error cases.
main1 :: IO ()
main1 = runResourceT $ do

  let
    -- A small helper that runs a Docker container and makes sure to stop it
    -- once we are done.
    docker :: TC.ToImage -> TC.ContainerRequest -> ResIO TC.Container
    docker image request = do
      (_, container) <- allocate (TC.run image request) TC.stop
      pure container

  -- Spin up the redis container
  redis <- docker TC.redis $ TC.defaultContainerRequest
    & TC.setExpose [6379]
    & TC.setName "redis"

  TC.waitUntilReady redis $ TC.waitForLogLine TC.Stdout
    ("Ready to accept connections" `isInfixOf`)

  -- connect to redis and have some fun...

  -- Note: No need for manual cleanup!
  -- TC.kill redis

```
