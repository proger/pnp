{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ViewPatterns #-}

-- | Exploratory implementation of a 2D tree.

module Optimize.Kdtree where

import           Data.Function (on)
import qualified Data.List as List
import           Data.Monoid
import           Data.Tree as Tree
import           Optimize.Branch
import           Optimize.Tree

import qualified Linear.Metric as M
import           Linear.V2

import           Debug.Trace

newtype Metric a = Metric a deriving (Show, Ord, Eq, Num)

vx, vy :: V2 a -> a
vx (V2 x _) = x
vy (V2 _ y) = y

metric a b = Metric (M.distance a b)

data X a
  = Xsplit a (Y a) (Y a)
  | Xnil
  deriving Show

xrec split _ (Xsplit n l r) = split n l r
xrec _ nil Xnil = nil

data Y a
  = Ysplit a (X a) (X a)
  | Ynil
  deriving Show

yrec split _ (Ysplit n l r) = split n l r
yrec _ nil Ynil = nil


type T2 a = X (V2 a)

sortPoints :: (Ord a, Floating a) => [V2 a] -> [V2 a]
sortPoints = List.sortBy (compare `on` M.norm)


pivot :: [a] -> ([a], a, [a])
pivot [] = error "pivot: empty list"
pivot xs = (left, m, right) where
  (left, m:right) = splitAt med xs
  med = length xs `div` 2


pfromList f get down xs = f m (down l) (down r) where
  (l, m, r) = pivot (List.sortBy (compare `on` get) xs)

fromList :: Ord a => [V2 a] -> T2 a
fromList = xlist

xlist :: Ord a => [V2 a] -> X (V2 a)
xlist [] = Xnil
xlist xs = pfromList Xsplit vx ylist xs

ylist :: Ord a => [V2 a] -> Y (V2 a)
ylist [] = Ynil
ylist xs = pfromList Ysplit vy xlist xs

check :: (Num a, Ord a) => Bound (Metric a) -> V2 a -> Plane (V2 a) -> Bool
check Unknown _ _ = False
check (Bound m) center line
   = coord' line >= planeAdd (fset center line) m
  || coord' line >= planeAdd (fset center line) (-m)

--check' :: V2 a -> Plane (V2 a) -> Bound (Metric a) -> Bound (Metric a)
check' center line bound = traceShow ("check", line, bound, check bound center line) bound

-- (>>?>) :: Ord a => Bound a -> a -> Bound a

fi true false cond = if cond then true else false

cx vec (Xsplit n l r)
  = closest vec vx X n (fi (ydown l) (ydown r))
  where
    candidate = metric n vec
    ydown = yrec (\n l r -> cy vec (Ysplit n l r) >>?> candidate) (Bound candidate)

cy vec (Ysplit n l r)
  = closest vec vy Y n (fi (xdown l) (xdown r))
  where
    candidate = metric n vec
    xdown = xrec (\n l r -> cx vec (Xsplit n l r) >>?> candidate) (Bound candidate)

closest ::
  (Ord a, Ord a1, Num a1)
  => V2 a1
  -> (V2 a1 -> a)
  -> (V2 a1 -> Plane (V2 a1))
  -> V2 a1
  -> (Bool -> Bound (Metric a1))
  -> Bound (Metric a1)
closest vec coord plane node elim =
  check' (coord vec <= coord node)

  where
    check' test =
      let bound' = elim test in
      if check bound' vec (plane node) then elim (not test) else bound'


nearest :: T2 a -> [V2 a]
nearest = undefined

sample :: [V2 Double]
sample = [V2 2 3, V2 5 4, V2 9 6, V2 4 7, V2 8 1, V2 7 2]

data Plane a = X a | Y a deriving (Show, Functor, Ord, Eq)

coord :: Plane (V2 a) -> a
coord (X v) = vx v
coord (Y v) = vy v

coord' :: Plane (V2 a) -> Plane a
coord' (X v) = X (vx v)
coord' (Y v) = Y (vy v)

planeAdd :: Num a => Plane (V2 a) -> Metric a -> Plane a
planeAdd plane (Metric m) = fset (coord plane + m) plane

anyp (X a) = a
anyp (Y a) = a

unfold x = case x of
  Left (Xsplit n Ynil Ynil)  -> (X n, [])
  Left (Xsplit n Ynil r)     -> (X n, [Right r])
  Left (Xsplit n l Ynil)     -> (X n, [Right l])
  Left (Xsplit n l r)        -> (X n, [Right l, Right r])
  Left Xnil                  -> error "wat"
  Right (Ysplit n Xnil Xnil) -> (Y n, [])
  Right (Ysplit n Xnil r)    -> (Y n, [Left r])
  Right (Ysplit n l Xnil)    -> (Y n, [Left l])
  Right (Ysplit n l r)       -> (Y n, [Left l, Left r])
  Right Ynil                 -> error "wat"

--inBall :: (Num a, Ord a) => V2 a -> Metric a -> Plane a -> Bool
inBall center radius plane = {-traceShow ("inBall", coord (fset center plane), planeAdd (fset center plane) radius, planeAdd (fset center plane) (-radius)) $ -} plane >= planeAdd (fset center plane) radius
                             || plane >= planeAdd (fset center plane) (-radius)

--metricTree :: Floating a => V2 a -> Tree (Plane (V2 a)) -> Tree (Plane (a, Metric a))

metricTree' vec = foldTree go where --
  go v xs -- = root (metric (anyp v) vec) xs
          = root (fset (coord v,
                        vec,
                        metric (anyp v) vec,
                        (fset (coord v) v),
                        inBall vec (metric (anyp v) vec) (fset (coord v) v)) v) xs

metricTree :: Floating a => V2 a -> Tree (Plane (V2 a)) -> Tree (Plane (V2 a), Metric a)
metricTree vec = foldTree go where --
  go v xs = root (v, metric (anyp v) vec) xs


fset = fmap . const
