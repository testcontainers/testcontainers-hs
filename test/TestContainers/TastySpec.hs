{-# LANGUAGE OverloadedStrings #-}
module TestContainers.TastySpec(main, test_all) where

import           Test.Tasty
import           Test.Tasty.HUnit
import           TestContainers.Tasty (MonadDocker, containerRequest, redis,
                                       run, setExpose, setWaitingFor,
                                       waitUntilMappedPortReachable,
                                       waitUntilTimeout, withContainers, (&))


containers1
  :: MonadDocker m => m ()
containers1 = do
  _redisContainer <- run $ containerRequest redis
    & setExpose [ 6379 ]
    & setWaitingFor (waitUntilTimeout 30 $
                      waitUntilMappedPortReachable 6379)
  pure ()


main :: IO ()
main = defaultMain test_all

test_all :: TestTree
test_all = testGroup "TestContainers tests"
  [
    withContainers containers1 $ \setup ->
      testGroup "Multiple tests"
      [
        testCase "test1" $ do
          setup
          return ()
      , testCase "test2" $ do
          setup
          return ()
      ]
  ]
