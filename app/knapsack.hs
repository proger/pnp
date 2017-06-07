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

ks_100_0 =
  (100, 3190802):
  (1491, 3882):
  (399, 1298):
  (77, 654):
  (969, 2638):
  (8485, 20670):
  (55, 610):
  (1904, 4908):
  (703, 2106):
  (657, 2014):
  (932, 2564):
  (1201, 3302):
  (1697, 4494):
  (462, 1424):
  (1201, 3302):
  (111632, 267364):
  (9044, 21988):
  (147380, 352660):
  (31852, 76604):
  (9044, 21988):
  (9300, 22700):
  (8660, 21020):
  (174684, 418068):
  (19844, 47788):
  (9044, 21988):
  (1635, 4370):
  (62788, 150476):
  (6932, 16964):
  (6308, 15516):
  (50, 600):
  (4600, 11300):
  (565204, 1351508):
  (7463, 18226):
  (2988, 7476):
  (9044, 21988):
  (9044, 21988):
  (4040, 9980):
  (137732, 329764):
  (7150, 17400):
  (9300, 22700):
  (177, 854):
  (372, 1244):
  (499, 1498):
  (15108, 36516):
  (11108, 26916):
  (2468, 6236):
  (1133, 3166):
  (1490, 3880):
  (865, 2430):
  (2468, 6236):
  (2468, 6236):
  (5974, 14648):
  (5972, 14644):
  (9532, 23164):
  (1872, 4844):
  (3964, 9828):
  (2799, 7098):
  (527708, 1261916):
  (7212, 17724):
  (3002, 7504):
  (21004, 50708):
  (47728, 114556):
  (565204, 1351508):
  (100600, 240900):
  (118920, 284740):
  (2822, 7144):
  (612, 1924):
  (6324, 15548):
  (9508, 23116):
  (9268, 22636):
  (11636, 28172):
  (210708, 504116):
  (2176944, 5204588):
  (930, 2560):
  (4481, 11062):
  (50, 600):
  (112, 724):
  (14434, 34968):
  (0, 500):
  (248, 996):
  (48, 596):
  (820, 2340):
  (278, 1056):
  (643, 1986):
  (1413, 3726):
  (1408, 3716):
  (0, 500):
  (2581, 6662):
  (287, 1074):
  (2040, 5180):
  (289, 1078):
  (1380, 3660):
  (372, 1244):
  (0, 500):
  (472, 1444):
  (360, 1220):
  (0, 500):
  (622, 1944):
  (3504, 8708):
  (5924, 14548):
  (2784, 7068):
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
