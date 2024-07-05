{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TestContainers.HspecSpec (main, spec_all) where

import Test.Hspec
import TestContainers.Hspec
  ( TestContainer,
    containerPort,
    containerRequest,
    redis,
    run,
    withContainers,
  )

data ContainerPorts = ContainerPorts
  { redisPort :: Int
  }

containers1 :: TestContainer ContainerPorts
containers1 = do
  redisContainer <- run $ containerRequest redis
  pure
    ContainerPorts
      { redisPort = containerPort redisContainer "6379/tcp"
      }

main :: IO ()
main = hspec spec_all

spec_all :: Spec
spec_all =
  around (withContainers containers1) $
    describe "TestContainers tests" $
      it "test1" $ \ContainerPorts {} ->
        shouldBe () ()
