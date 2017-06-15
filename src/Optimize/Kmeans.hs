module Optimize.Kmeans where

import           Prelude hiding (map, zip, length)

import           Control.Monad.Primitive (PrimMonad, PrimState)
import           Data.Vector.Generic
import           Optimize.Linear
import           Optimize.Vector
import qualified System.Random.MWC as MWC

import qualified Data.Vector as V
import           Linear.Affine (Point(..))
import           Optimize.Diagram

type Assignment = Int

type KMeans v w f a =
  ( Ord a
  , Metric f
  , Floating a
  , Vector w Assignment
  , Vector w (f a, Assignment)
  , Vector w (f a)
  , Vector v (w (f a))
  , Vector v (f a)
  , Vector v a
  , Foldable w
  )

kmeans ::
  (KMeans v w f a, PrimMonad m)
  => MWC.Gen (PrimState m)
  -> Int
  -> w (f a)
  -> m [(v (w (f a)), v (f a))] -- ^ infinite list of clusters and centers
kmeans gen k xs = do
  init <- randomClusters gen xs k
  return $ iterate (lloyd k xs) (fromList init, map mean (fromList init))

closestIndex ::
  (Floating a, Metric f, Vector v (f a), Vector v a, Ord a)
  => f a
  -> v (f a)
  -> Int
closestIndex pt centers = minIndex (map (distance pt) centers)

assign ::
  (Ord a, Vector v (f a), Vector v a, Vector w Assignment,
   Vector w (f a), Metric f, Floating a)
  => w (f a)
  -> v (f a)
  -> w Int
assign pts centers = map (\pt -> closestIndex pt centers) pts

cluster ::
  (Vector v (w point), Vector w Assignment, Vector w (point, Assignment),
   Vector w point)
  => Int -> w point -> w Int -> v (w point)
cluster n pts assignments = generate n (\k -> imapMaybe (\_ -> f k) pairs)
  where
    pairs = zip pts assignments

    f n (pt, n') | n == n'   = Just pt
                 | otherwise = Nothing

mean :: (Vector v (f a), Additive f, Foldable v, Fractional a) => v (f a) -> f a
mean pts = sumV pts ^/ fromIntegral (length pts)


-- | Lloyd's algorithm update step.
lloyd :: KMeans v w f a => Int -> w (f a) -> (v (w (f a)), v (f a)) -> (v (w (f a)), v (f a))
lloyd k xs (_, centers) = (clusters, centers')
  where clusters = cluster k xs (assign xs centers)
        centers' = map mean clusters

nap :: Neat f a -> Point f a
nap (S p) = P p

spec :: IO _
spec = do
  gen :: MWC.GenIO <- MWC.initialize (V.fromList [4, 8, 15, 16, 23, 42, 55])
  vecs <- randomVectors @NeatR2 @V.Vector gen 100
  let k = 7
  clusterings <- kmeans gen k vecs
  let (clusters', centers) = clusterings !! 10
  plotClusters (fmap (map nap) (V.toList clusters')) (fmap nap (V.toList centers))
  return ()
