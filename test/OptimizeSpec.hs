module OptimizeSpec where

import Test.Hspec
import Test.QuickCheck

import Optimize

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
  it "evals something" $ (1+1) `shouldBe` 2
  it "prints something" $ repl `shouldReturn` ()
  it "is a stub" $ do
    pending

  describe "read" $ do
    context "when used with ints" $ do
      it "is inverse to show" $ property $
        \x -> (read . show) x == (x :: Int)
