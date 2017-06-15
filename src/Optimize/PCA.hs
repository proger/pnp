-- | https://arxiv.org/abs/1404.1100

module Optimize.PCA where

import           Control.Monad (filterM)
import           Control.Monad.Primitive
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Numeric.LinearAlgebra
import           Numeric.LinearAlgebra.Data
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC


-- 3 variables : x, y, z ( rows ), 4 observations
t = (3><4) [ 0.1,  0, -0.05, 0,
             0.9,  1,   1.1, 1,
             2.05, 2,     2, 2
           ]

tt = (2><5) [ 2.0,  4, 6, 8, 10,
              2.0,  4, 6, 8, 10 ]

rowMeans t = asColumn (fst (meanCov (tr t)))

subtractMean t = t - repmat (rowMeans t) 1 (cols t)

y t = (tr (subtractMean t)) / (sqrt $ fromIntegral (cols t - 1))

-- | Find @pcs@ ("rotate" @x@) such that @signals = pcs <> x@
--   where @cov signals = (1 / pred n) * (signals <> tr signals)@ is diagonal
--
-- @svd@ of Y gives eigenvectors of @cov x = tr y <> y@
-- columns of V from UDV'=svd Y are principal components of X
--

pca x = (signals, pcs, variances)
  where
    (u, s, pcs) = svd (y x)
    variances = diag s * diag s
    signals = pcs <> x

-- |
-- >>> iopca tt
-- reprojected
-- 2x5
-- 0.00  0.00  0.00   0.00   0.00
-- 2.83  5.66  8.49  11.31  14.14
-- principal components
-- 2x2
-- 0.71  -0.71
-- 0.71   0.71
-- variances
-- 2x2
-- 20.00  0.00
--  0.00  0.00
iopca t = do
  let (s,p,v) = pca t
  putStrLn "reprojected"
  disp 2 s
  putStrLn "principal components"
  disp 2 p
  putStrLn "variances"
  disp 2 v


