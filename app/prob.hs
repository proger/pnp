{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}

import Prelude hiding (product)
import Control.Applicative
import Control.DeepSeq (NFData)
import Control.Monad (ap, void, when, guard)
import qualified Criterion.Main as Crit
import Data.Function (on)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ratio ((%), approxRational)
import GHC.Generics (Generic)
import System.Environment (getArgs)
import System.Random (Random(..), getStdGen)
import Test.QuickCheck (quickCheckAll)
import qualified Test.QuickCheck as QC

-- Following the steps of
-- http://dl.acm.org/citation.cfm?id=1114013
-- FUNCTIONAL PEARLS
-- Probabilistic Functional Programming in Haskell
-- MARTIN ERWIG and STEVE KOLLMANSBERGER

newtype Probability = P Float deriving (Show, Num, Fractional, Eq, Ord)
unP (P f) = f

instance Random Probability where
  randomR (P lo, P hi) g = let (p, g') = randomR (lo, hi) g in (P p, g')
  random = randomR (P 0.0, P 1.0)

-- "categorical" in anglican
newtype Dist a = D [(a, Probability)] deriving Show
unD (D f) = f

-- * The returned probability should be P 1.0.
checkD :: Dist a -> Probability
checkD = sum . map snd . unD

cancel :: Dist (Maybe a) -> Dist a
cancel (D xs) = reweight (D [(x, p) | (Just x, p) <- xs])

reweight (D ds) = D (map weight ds) where
  weight (x, w) = (x, w / sum)
  sum = List.foldl' (\w1 (_,w2) -> w1+w2) 0 ds

-- * If p is a random probability return a random element from Dist.
sampleDist :: Dist a -> Probability -> a
sampleDist (D d) p = go p d where
  go p ((x,q):xs) | p <= q || null xs = x
                  | otherwise         = go (p-q) xs

type Spread a = [a] -> Dist a

type Event a = a -> Bool

type Trans a b = a -> Dist b
type Trans1 a = Trans a a

-- normal, enum :: Spread

certainly :: a -> Dist a
certainly x = D [(x, P 1)]

impossible = D []

uniform :: Spread a
uniform xs = D [(x, p) | x <- xs]
  where p = P (1.0 / fromIntegral (length xs))

gaussian mu sigma x = norm * exp (-0.5*t) where
  norm = 1 / ((sqrt (2*pi)) * sigma)
  t = ((x - mu) / sigma)^2

die = uniform [1..6]

(??) :: Event a -> Dist a -> Probability
(??) test = P . sum . map (unP . snd) . filter (test . fst) . unD

prop_die = (\case 3 -> True; _ -> False) ?? die == (P $ 1.0/6.0)

-- Constructing distributions:

-- Independent events: take all possible combinations and multiply probabilities,
-- as P(A,B) = P(A)*P(B) = P(A|B) = P(B|A)
joinWith :: (a -> b -> c) -> Dist a -> Dist b -> Dist c
joinWith f (D a) (D b) = D [(f x y, p*q) | (x,p) <- a, (y,q) <- b]

prod :: Dist a -> Dist b -> Dist (a, b)
prod = joinWith (,)


-- If the second event depends on the first,
-- it must be represented by a function that accepts values of the distribution
-- produced by the first event.
-- In other words, whereas the first event can be represented by a Dist a value,
-- the second event should be a function of type (a -> Dist b).
-- sigfpe: "a number attached to some data that gives a level of trust"
-- http://www.randomhacks.net.s3-website-us-east-1.amazonaws.com/2007/02/21/refactoring-probability-distributions/

-- * P(Y,X) = P(Y|X)P(X) -- product rule
product :: (x -> Dist y) -> Dist x -> Dist (y, x)
product f d = D [ ((y, x), pyx*px)
                | (x,      px  {-P(X)  -}) <- unD d
                , (y,      pyx {-P(Y|X)-}) <- unD (f x) {- conditioning -}]

-- * P(Y) = sum_X P(Y,X) -- sum rule
marginalize1 :: Ord y => Dist (y,x) -> Dist y
marginalize1 = marginalize fst

-- Like (??) but for every possible Event
marginalize :: Ord proj => (product -> proj) -> Dist product -> Dist proj
marginalize project joint = D $
  map (\xs -> (fst (head xs), foldr (+) 0 (map snd xs))) $
  List.groupBy ((==) `on` fst) $ List.sortBy (compare `on` fst) $ unD (fmap project joint)

-- bayes inferBox == inferFruit / marginalize fst inferFruit

probsum :: Dist t -> Probability
probsum (D d) = sum [p | (_,p) <- d]

-- * P(X|Y) = (P(Y|X)P(X))/P(Y) = P(Y,X) / P(Y)
bayes :: Dist (y,x) -> Dist y -> Dist (y,x) -- -> (y -> Dist x)
bayes (D joint) norm = D [(x, p/ps) | (x,p) <- joint] where ps = probsum norm

instance Functor Dist where fmap f (D d) = D [(f x, p) | (x,p) <- d]
instance Applicative Dist where pure = return; (<*>) = ap
instance Monad Dist where
  return = certainly
  fail _ = impossible
  d >>= f = fmap fst (product f d)

-- | All possible (n choose 1) choices from xs.
selections :: Eq a => [a] -> [(a, [a])]
selections xs = [(x, List.delete x xs) | x <- xs]

selectMany :: Eq a => Int -> [a] -> Dist ([a], [a])
selectMany 0 c = return ([], c)
selectMany n c = do (x, c1) <- uniform (selections c)
                    (xs, c2) <- selectMany (n-1) c1
                    return (x:xs, c2)

select :: Eq a => Int -> [a] -> Dist [a]
select n = fmap (reverse . fst) . selectMany n

-- (==[1,2,3]) ?? select 3 [1,1,2,2,3,3]
-- approxRational 6.666667e-2  0.00001

data Vals = L | M | N | O deriving (Show, Ord, Eq, Generic)
instance NFData Vals
simD = D [(L, P 0.1), (M, P 0.2), (N, P 0.3), (O, P 0.4)]

count :: (Ord a) => [a] -> Map a Int
count = Map.fromListWith (+) . map (, 1)

count' :: (Ord a) => [a] -> [(a, Int)]
count' = map (\l -> (head l, length l)) . List.group . List.sort -- slower


probs :: IO [Probability]
probs = fmap randoms getStdGen

data Box = Red | Blue deriving (Show, Ord, Eq, Generic)
box = D [(Red, P 0.4), (Blue, P 0.6)]

data Fruit = Apple | Orange deriving (Show, Ord, Eq, Generic)
inside Red = D [(Apple, P 0.25), (Orange, P 0.75)]
inside Blue = D [(Apple, P 0.75), (Orange, P 0.25)]

-- p(fruit): "what is the overall probability of picking an orange?"
-- (== Apple) ?? inferFruit == P 0.55
-- only sum rule used here
inferFruit = do
  b <- box
  f <- inside b
  --return f
  return (f, b)

-- P(Box == Blue | Fruit == Orange)
-- "what is the probability I picked the blue bin, given I picked an orange?"
-- (== Just Blue) ?? inferBox Orange
-- expect 1/3
-- http://www.randomhacks.net/2007/02/22/bayes-rule-and-drug-tests/
-- > As Russel and Norvig point out (chapter 13), cancelling out the impossible worlds and normalizing the remaining probabilities is equivalent to Bayes' rule.
inferBox f' = cancel {-bayes-} $ do
  b <- box
  f <- inside b
  -- conditioning
  return $ if f == f' then Just b else Nothing

-- * plumbing


setupEnv =
  fmap (zipWith ($) (repeat (sampleDist simD))) probs  >>= return . take 1000000

benchmarks = [
  -- notice the lazy pattern match here!
  Crit.env setupEnv $ \ ~(elements) -> Crit.bgroup "counts"
    [ Crit.bench "Map.fromListWith" $ Crit.whnf count elements
    , Crit.bench "group.sort" $ Crit.whnf count' elements
    ]
  ]



return []
qcAll = $quickCheckAll

main = do
  args <- getArgs
  case args of
    "qc":_ -> void $ qcAll
    _ -> void $ Crit.defaultMain benchmarks
