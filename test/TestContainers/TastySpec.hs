module TestContainers.TastySpec(main, test_all) where

import           Test.Tasty
import           Test.Tasty.HUnit
import           TestContainers.Tasty (MonadDocker, defaultContainerRequest,
                                       redis, run, withContainers)


containers1
  :: MonadDocker m => m ()
containers1 = do
  _ <- run redis defaultContainerRequest
  pure ()


main :: IO ()
main = defaultMain test_all

test_all :: TestTree
test_all = testGroup "TestContainers tests"
  [
    withContainers containers1 $ \setup ->
      testCase "test1" $ do
        setup
        return ()
  ]
