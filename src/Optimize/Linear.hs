{-# LANGUAGE FlexibleInstances #-}
module Optimize.Linear (module Optimize.Linear, module Linear) where

import           Control.Monad.Primitive
import           Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import           Linear
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC

import           Test.Hspec
import           Test.QuickCheck

minBoundR, maxBoundR :: Num a => a
minBoundR = 0
maxBoundR = 100

fromInt :: Int -> Double
fromInt = fromIntegral

class NormalVariate a where
  normal :: PrimMonad m => a -> a -> MWC.Gen (PrimState m) -> m a

newtype Neat f a = S (f a)
  deriving ( Eq, Ord, Show, Read, Monad, Functor, Applicative, Foldable
           , Additive, Metric
           , Fractional , Num, Epsilon )

type NeatR2 = Neat V2 Double

unitR2 = S (V2 1 1)

instance MWC.Variate (Neat V2 Double) where
  uniform gen = do
    x <- MWC.uniformR (minBoundR, maxBoundR) gen
    y <- MWC.uniformR (minBoundR, maxBoundR) gen
    return (S (V2 (fromInt x) (fromInt y)))
  uniformR (S (V2 x y), S (V2 x' y')) gen = do
    x <- MWC.uniformR (x, x') gen
    y <- MWC.uniformR (y, y') gen
    return (S (V2 x y))


instance NormalVariate (Neat V2 Double) where
  normal (S (V2 xm ym)) (S (V2 xs ys)) gen = do
    x <- MWC.normal xm xs gen
    y <- MWC.normal ym ys gen
    return (S (V2 x y))


type NeatR3 = Neat V3 Double

instance MWC.Variate (Neat V3 Double) where
  uniform gen = do
    x <- MWC.uniformR (minBoundR, maxBoundR) gen
    y <- MWC.uniformR (minBoundR, maxBoundR) gen
    z <- MWC.uniformR (minBoundR, maxBoundR) gen
    return (S (V3 x y z))
  uniformR (S (V3 x y z), S (V3 x' y' z')) gen = do
    x <- MWC.uniformR (x, x') gen
    y <- MWC.uniformR (y, y') gen
    z <- MWC.uniformR (z, z') gen
    return (S (V3 x y z))

-- |
-- >>> MWC.create >>= \gen -> randomVectors @BoundedR3 @Vector gen 10
randomVectors :: (MWC.Variate a, G.Vector v a, PrimMonad m) => MWC.Gen (PrimState m) -> Int -> m (v a)
randomVectors gen n = G.replicateM n (MWC.uniform gen)

-- | Split the vector into @n@ (almost) evenly-sized groups.
randomClusters :: (G.Vector v a, PrimMonad m) => MWC.Gen (PrimState m) -> v a -> Int -> m [v a]
randomClusters gen xs n = do
  xs' <- MWC.uniformShuffle xs gen
  return [G.unsafeSlice s e xs' | (s,e) <- slices (grouping (G.length xs') n)]

slices :: Num a => [a] -> [(a, a)]
slices xs = zip (scanl (+) 0 xs) xs

grouping :: Int -> Int -> [Int]
grouping len n = (len - (n-1)*piece):(replicate (n-1) piece)
  where piece = len `div` n





spec = hspec $ do
  context "sum of grouping" $
    it "is the same as total len" $
      property (\(len :: Int, Positive n) -> let gp = grouping len n in sum gp == len)
