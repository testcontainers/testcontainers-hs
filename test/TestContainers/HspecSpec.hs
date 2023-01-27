{-# LANGUAGE FlexibleContexts #-}

module TestContainers.HspecSpec (main, spec_all) where

import Test.Hspec
import TestContainers.Hspec
  ( TestContainer,
    containerRequest,
    redis,
    run,
    withContainers,
  )

containers1 ::
  TestContainer ()
containers1 = do
  _ <- run $ containerRequest redis
  pure ()

main :: IO ()
main = hspec spec_all

spec_all :: Spec
spec_all =
  around (withContainers containers1) $
    describe "TestContainers tests" $
      it "test1" $
        shouldBe ()
