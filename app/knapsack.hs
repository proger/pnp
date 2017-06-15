{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms, NoMonomorphismRestriction #-}

import Discrete

import Data.List (sortBy)
import Data.Function (on)
import Data.Tree (Tree)
import qualified Data.Tree as Tree
import Data.Maybe (catMaybes)
import Data.Tree.Pretty

import Debug.Trace

phtree = putStrLn . Tree.drawTree . fmap show
pvtree = putStrLn . drawVerticalTree . fmap show

pattern Tree a b = Tree.Node a b

newtype Value = Value Int deriving (Show, Ord, Eq, Num)
newtype Estimate = Estimate Int deriving (Show, Ord, Eq, Num)
newtype Weight = Weight Int deriving (Show, Ord, Eq, Num)

unValue (Value x) = x

data Knapsack = Knapsack Estimate Value Weight [(Value, Weight)] deriving (Show, Eq)

ks_5 =
  (5, 7):
  (16, 2):
  (19, 3):
  (23, 4):
  (28, 5):
  (1, 5):
  []

-- taken from https://www.coursera.org/learn/discrete-optimization
-- format:
--
-- N      max-capacity
-- value1 weight1
-- value2 weight2
-- ...    ...
-- valueN weightN
--
ks_30_0 =
  (30, 100000):
  (90000, 90001):
  (89750, 89751):
  (10001, 10002):
  (89500, 89501):
  (10252, 10254):
  (89250, 89251):
  (10503, 10506):
  (89000, 89001):
  (10754, 10758):
  (88750, 88751):
  (11005, 11010):
  (88500, 88501):
  (11256, 11262):
  (88250, 88251):
  (11507, 11514):
  (88000, 88001):
  (11758, 11766):
  (87750, 87751):
  (12009, 12018):
  (87500, 87501):
  (12260, 12270):
  (87250, 87251):
  (12511, 12522):
  (87000, 87001):
  (12762, 12774):
  (86750, 86751):
  (13013, 13026):
  (86500, 86501):
  (13264, 13278):
  (86250, 86251):
  []

build :: [(Int, Int)] -> (Weight, [(Value, Weight)])
build ((_nitems, capacity):ds)
  = (Weight capacity, map (\(v, w) -> (Value v, Weight w)) ds)

-- we want to maximize value

empty :: Estimate -> Knapsack
empty est = Knapsack est 0 0 []

insert :: Estimate -> Value -> Weight -> Knapsack -> Knapsack
insert est v w (Knapsack _ tv tw xs) = Knapsack (est + Estimate (unValue value)) value (w+tw) ((v,w):xs)
  where
    value = v+tv

deduct (Value val) (Knapsack (Estimate est) v w xs) = Knapsack (Estimate (est-val)) v w xs

estimate :: [(Value, Weight)] -> Estimate
estimate = Estimate . sum . map (unValue . fst)

bruteforce :: Weight -> [(Value, Weight)] -> Knapsack -> Tree Knapsack
bruteforce _ [] s = Tree s []
bruteforce cap ((value, weight):xs) start
  = Tree start (catMaybes [yes, no])
  where
    cap' = cap - weight
    yes = if cap' >= 0
          then Just (bruteforce cap' xs (insert (estimate xs) value weight start))
          else Nothing
    no = Just (bruteforce cap xs (deduct value start))

wpk (Knapsack (Estimate e) (Value v) (Weight w) _) = e -- fromIntegral v / fromIntegral w

instance Ord Knapsack where
  -- flipped
  (<=) ka kb = not (wpk ka < wpk kb)



go ks = let (w, xs) = build ks in bruteforce w xs (empty (estimate xs))
