{-# LANGUAGE PatternSynonyms, NoMonomorphismRestriction #-}

import Discrete

import Debug.Trace
import Data.Tree (Tree)
import qualified Data.Tree as Tree
import Data.Maybe (catMaybes)
import Data.Tree.Pretty

pattern Tree a b = Tree.Node a b

phtree = putStrLn . Tree.drawTree . fmap show
pvtree = putStrLn . drawVerticalTree . fmap show

-- the graph

data Node
  = A | B | C | D | E | F | G | H | I | J | K | L | M | N
  deriving (Show, Enum, Bounded, Eq)

enum :: (Bounded a, Enum a) => [a]
enum = [minBound..maxBound]

-- | like takeWhile with a negated predicate, keeps the first match
waitFor _ [] = []
waitFor p (x:xs)
  | p x  = x : []
  | True = x : waitFor p xs


-- WANT:
-- shortest A N (branch and bound)
-- WANT:
-- shortest A N (dynamic programming)


edge from to =
  case (from,to) of
    a1@(A,B) -> Just 4
    a2@(A,E) -> Just 6
    a3@(A,D) -> Just 7
    b1@(B,E) -> Just 6
    b2@(B,I) -> Just 3
    b3@(B,F) -> Just 2
    c1@(C,E) -> Just 8
    c2@(C,F) -> Just 6
    d1@(D,C) -> Just 3
    d2@(D,I) -> Just 2
    d3@(D,G) -> Just 8
    e1@(E,H) -> Just 7
    e2@(E,F) -> Just 4
    f1@(F,H) -> Just 2
    f2@(F,G) -> Just 3
    g1@(G,H) -> Just 9
    g2@(G,J) -> Just 7
    h1@(H,K) -> Just 6
    h2@(H,L) -> Just 6
    i1@(G,I) -> Just 7
    i2@(I,L) -> Just 2
    i3@(I,M) -> Just 3
    j1@(J,I) -> Just 6
    j2@(J,M) -> Just 7
    j3@(J,L) -> Just 2
    k1@(K,N) -> Just 3
    l1@(L,N) -> Just 7
    l2@(L,M) -> Just 8
    m1@(M,N) -> Just 3
    _ -> Nothing

reachable :: Node -> [Node]
reachable from = catMaybes [fmap (const to) (edge from to) | to <- enum]


type Trace = [Node]

data Path = Path Int Trace deriving (Show, Eq)

instance Ord Path where
  (Path a _) <= (Path b _) = a <= b


bf :: Int -> Trace -> Node -> Node -> Tree Path
bf cost trace to from = Tree (Path cost trace') subs
  where
    subs = case to == from of
      True -> []
      False -> map build (reachable from)

    trace' = from:trace
    build next = bf (cost + edge' from next) trace' to next

    edge' f t = maybe (error ("missing edge: " ++ show from ++ " -> " ++ show to)) id (edge f t)

bruteforce :: Node -> Node -> Tree Path
bruteforce = bf 0 []
