module Optimize.KdtreeSpec where

import Optimize.Branch
import Optimize.Kdtree
import Optimize.Vector
import Optimize.Linear
import Test.Hspec
import Test.QuickCheck


spec :: Spec
spec = do
  it "does nearest neighbor lookup" $
    (cx (V2 6.63 7.02) (fromList minisample)) `shouldBe` (Bound (Metric 2.5801744127093422))
  it "does other nearest neighbor lookup" $
    (cx (V2 7.51 2) (fromList minisample)) `shouldBe` (Bound (Metric 0.5099999999999998))

minisample :: [V2 Double]
minisample = [V2 2 3, V2 5 4, V2 9 6, V2 4 7, V2 8 1, V2 7 2]



-- bare = unfoldTree unfold (Left (fromList sample))
