module TestContainers.HspecSpec(main, spec_all) where

import           Control.Monad.IO.Class (liftIO)
import           Test.Hspec
import           TestContainers.Hspec   (MonadDocker, defaultContainerRequest,
                                         redis, run, withContainers)


containers1
  :: MonadDocker m => m ()
containers1 = do
  _ <- run redis defaultContainerRequest
  pure ()


main :: IO ()
main = hspec spec_all

spec_all :: Spec
spec_all = around (withContainers containers1) $
  describe "TestContainers tests" $
    it "test1" $ shouldBe ()
