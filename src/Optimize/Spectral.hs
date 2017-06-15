module Optimize.Spectral where

import           Control.Monad (filterM)
import           Control.Monad.Primitive
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Numeric.LinearAlgebra
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC


randomEdges :: MWC.GenIO -> [a] -> IO [(a, a)]
randomEdges gen nodes = filterM (\_ -> MWC.uniform gen) (pairs nodes)



pairs (h:t) = [(h,x) | x <- t] ++ pairs t
pairs [] = []



-- generate random connections

spec = do
  gen  :: MWC.GenIO <- MWC.initialize (V.fromList [4, 8, 15, 16, 23, 42, 55])
  randomEdges gen [1..6]
