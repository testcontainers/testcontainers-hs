{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module TestContainers.TastySpec (main, test_all) where

import Data.Text.Lazy (isInfixOf)
import Test.Tasty
import Test.Tasty.HUnit
import TestContainers.Tasty
  ( Pipe (Stdout),
    TestContainer,
    containerRequest,
    fromBuildContext,
    fromTag,
    redis,
    run,
    setExpose,
    setRm,
    setWaitingFor,
    waitForHttp,
    waitForLogLine,
    waitUntilMappedPortReachable,
    waitUntilTimeout,
    withContainers,
    (&),
  )

containers1 ::
  TestContainer ()
containers1 = do
  _redisContainer <-
    run $
      containerRequest redis
        & setExpose [6379]
        & setWaitingFor
          ( waitUntilTimeout 30 $
              waitUntilMappedPortReachable 6379
          )

  _rabbitmq <-
    run $
      containerRequest (fromTag "rabbitmq:3.8.4")
        & setRm False
        & setWaitingFor (waitForLogLine Stdout (("completed with" `isInfixOf`)))

  _nginx <-
    run $
      containerRequest (fromTag "nginx:1.23.1-alpine")
        & setExpose [80]
        & setWaitingFor
          ( waitUntilTimeout 30 $
              waitForHttp 80 "/" [200]
          )

  _test <- run $ containerRequest (fromBuildContext "./test/container1" Nothing)

  pure ()

main :: IO ()
main = defaultMain test_all

test_all :: TestTree
test_all =
  testGroup
    "TestContainers tests"
    [ withContainers containers1 $ \setup ->
        testGroup
          "Multiple tests"
          [ testCase "test1" $ do
              setup
              return (),
            testCase "test2" $ do
              setup
              return ()
          ]
    ]
