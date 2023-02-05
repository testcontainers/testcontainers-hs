{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module TestContainers.TastySpec (main, test_all) where

import Data.Text.Lazy (isInfixOf)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit
import TestContainers.Tasty
  ( Pipe (Stdout),
    TestContainer,
    consoleLogConsumer,
    containerRequest,
    createNetwork,
    fromBuildContext,
    fromTag,
    networkRequest,
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
    withFollowLogs,
    withNetwork,
    (&),
  )

containers1 ::
  TestContainer ()
containers1 = do
  net <-
    createNetwork networkRequest

  _redisContainer <-
    run $
      containerRequest redis
        & setExpose [6379]
        & withNetwork net
        & setWaitingFor
          ( waitUntilTimeout 30 $
              waitUntilMappedPortReachable 6379
          )

  _rabbitmq <-
    run $
      containerRequest (fromTag "rabbitmq:3.8.4")
        & setRm False
        & setExpose [5672]
        & withNetwork net
        & withFollowLogs consoleLogConsumer
        & setWaitingFor
          ( waitForLogLine Stdout (("completed with" `isInfixOf`))
              <> waitUntilMappedPortReachable 5672
          )

  _nginx <-
    run $
      containerRequest (fromTag "nginx:1.23.1-alpine")
        & setExpose [80]
        & withNetwork net
        & setWaitingFor
          ( waitUntilTimeout 30 $
              waitForHttp 80 "/" [200]
          )

  _jaeger <-
    run $
      containerRequest (fromTag "jaegertracing/all-in-one:1.6")
        & setExpose ["5775/udp", "6831/udp", "6832/udp", "5778", "16686/tcp"]
        & withNetwork net
        & setWaitingFor
          (waitForHttp "16686/tcp" "/" [200])

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
