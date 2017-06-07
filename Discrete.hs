module Discrete where

import Debug.Trace
import Data.Tree

data Bound a = Bound a
             | Unknown
             deriving (Show, Eq)



a `betterThan` b = case (a, b) of
  (Unknown, Unknown) -> False
  (Unknown, x)       -> False
  (x, Unknown)       -> True
  (Bound x, Bound y) -> x <= y

-- | Unlike Maybe's Ord this one steers away from Unknown
instance (Eq a, Ord a) => Ord (Bound a) where
  compare Unknown Unknown     = EQ
  compare Unknown x           = GT
  compare x Unknown           = LT
  compare (Bound x) (Bound y) = compare x y


-- | Finds the minimum leaf in the tree using depth-first branch and bound.
branch :: (Show a, Ord a) => Tree a -> Bound a
branch = branch' Unknown

branch' :: (Show a, Ord a) => Bound a -> Tree a -> Bound a
branch' bound (Node root subs) =
  case subs of
    [] -> Bound root
    _  -> foldr f bound subs
  where
    -- f sub@(Node r _) b =
    --   case Bound r `betterThan` b of
    --     True  -> branch' b sub
    --     False -> --traceShow ("got bound", b, "prune", sub)
    --              b -- prune

    f sub@(Node r _) b
      | Bound r <= b = branch' b sub
      | otherwise    = b -- prune
