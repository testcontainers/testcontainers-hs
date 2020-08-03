module TestContainers.TastySpec(main, test_all) where

import           Test.Tasty
import           Test.Tasty.HUnit
import           TestContainers.Tasty (MonadDocker, containerRequest, redis,
                                       run, setExpose, withContainers, (&))


containers1
  :: MonadDocker m => m ()
containers1 = do
  _ <- run $ containerRequest redis
    & setExpose [1234]
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
