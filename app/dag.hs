{-# LANGUAGE NoMonomorphismRestriction, GeneralizedNewtypeDeriving, ScopedTypeVariables, ViewPatterns #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

import Data.Maybe (catMaybes, fromJust)
import Data.List (sortBy, find)
import Control.Monad
import Data.Tree as Tree
import Data.Tree.Pretty
import Debug.Trace

import qualified Data.PQueue.Min as PM

-- trees

root = Tree.Node
leaf = flip root []

foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree f = go where
    go (Tree.Node x ts) = f x (map go ts)

-- priority queue wrapper

type Pq = PM.MinQueue

pqmerge :: Ord a => Pq a -> Pq a -> Pq a
pqmerge = PM.union

pqinsert :: Ord a => a -> Pq a -> Pq a
pqinsert = PM.insert

pqmergeAll :: Ord a => Pq a -> [Pq a] -> Pq a
pqmergeAll = foldr pqmerge

pop :: Ord a => Pq a -> (a, Pq a)
pop = PM.deleteFindMin

pqempty = PM.empty

pqadd :: (Ord a, Foldable t) => Pq a -> t a -> Pq a
pqadd = foldr pqinsert

pqfind :: Ord a => (a -> Bool) -> Pq a -> Maybe a
pqfind pred = find pred . PM.toAscList

unPq = PM.toAscList

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

edge' from to = case edge from to of
  Just c -> c
  Nothing -> error $ "missing edge: " ++ show from ++ " -> " ++ show to

minCost :: Node -> Node -> Node -> Ordering
minCost from a b = compare (edge from a) (edge from b)

reachable :: Node -> [Node]
reachable from = catMaybes [fmap (const to) (edge from to) | to <- enum]

data Costs a = Costs Int a deriving (Show)

trapCosts op (Costs c x) = (c,x) --traceShow ("trapCosts", op, c, x) (c,x)

--instance Eq a => Eq (Costs a) where
  --(==) (Costs a x) (Costs b y) = (a,x) == (b,y)
instance (Show a, Eq a) => Eq (Costs a) where
  (==) (trapCosts "eq" -> (a,x)) (trapCosts "eq" -> (b,y)) = (a,x) == (b,y)

-- instance Eq a => Ord (Costs a) where
--   compare (Costs a _) (Costs b _) = compare a b

instance (Show a, Eq a) => Ord (Costs a) where
  compare (trapCosts "ord" -> (a,_)) (trapCosts "ord" -> (b,_)) = compare a b


-- paths :: Node -> [Path]
-- paths from = [Path from to (edge' from to) | to <- reachable from]

-- convention: f to from

anypath :: Node -> Node -> [Node]
anypath to = waitFor (== to) . iterate go where
  go from =
    head (reachable from)

bestfirst :: Node -> Node -> [Node]
bestfirst to = waitFor (== to) . iterate go where
  go from =
    head (sortBy (minCost from) (reachable from))


annotateCost f to from c0 next = f to next (c0 + edge' from next)

bruteforce :: Node -> Node -> Int -> Tree (Costs Node)
bruteforce to from cost =
  root (Costs cost from) (map (annotateCost bruteforce to from cost) (reachable from))

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ s [] = s
foldr' f s (x:xs) = f x (foldr' f s xs)

-- foldTree :: (a -> [b] -> b) -> Tree a -> b

-- | returns paths from leaves in depth-first manner
depthFirst :: Tree a -> [[a]]
depthFirst = depthFirst' []

-- depthFirst' :: [a] -> Tree a -> [(a, [a])]
-- depthFirst' trace (Tree.Node root subs) =
--   (root, trace):(concatMap (depthFirst' (root:trace)) subs)

depthFirst' :: [a] -> Tree a -> [[a]]
depthFirst' trace (Tree.Node root []) =
  [(root:trace)]
depthFirst' trace (Tree.Node root subs) =
  foldr (\subtree acc -> append (depthFirst' (root:trace) subtree) acc) [] subs

{-
data S a
  = Bound [Costs a]
  | Empty
  deriving Show


pappend :: (Show a, Eq a) => S a -> S a -> S a
pappend x Empty = x
pappend Empty x = x
pappend (Bound b) (Bound b') = Bound (head (sortBy compare [b, b']))

-- pappend l r = foldr (:) r l


dfw :: forall a. (Show a, Eq a) => [Costs a] -> S a -> Tree (Costs a) -> S a
dfw trace bound' (Tree.Node root []) =
  traceShow ("cand bound", root:trace) $
  Bound (root:trace)
dfw trace bound' (Tree.Node root subs) = foldr go bound' subs where


  go :: Tree (Costs a) -> S a -> S a

  go subtree@(Tree.Node (Costs c x) _) bound@(Bound (Costs s _:_))
    | s <= c = traceShow ("prune", subtree, "bound", bound) bound
    | otherwise = --traceShow ("in ", c, x, bound) $
        pappend (dfw (root:trace) bound subtree) bound

  go subtree@(Tree.Node (Costs c _) _) (Bound []) = error "wat"
  go subtree Empty = dfw (root:trace) Empty subtree
-}


data Bound a = Bound a
             | Unknown
             deriving (Show, Eq)

-- | Unlike Maybe's Ord this one steers away from Unknown
instance (Eq a, Ord a) => Ord (Bound a) where
  compare Unknown Unknown = EQ
  compare Unknown x = GT
  compare x Unknown = LT
  compare (Bound x) (Bound y) = compare x y


branch :: Ord a => Tree a -> Bound a
branch = branch' Unknown


branch' :: forall a. Ord a => Bound a -> Tree a -> Bound a
branch' bound (Tree.Node root subs) =
  case subs of
    [] -> Bound root
    _  -> foldr f bound subs
  where
    f sub@(Tree.Node r _) b
      | Bound r <= b = branch' b sub
      | otherwise    = b -- prune



-- repl = dfc [] Empty bigtree -- minitree'




append' l r = foldr (:) r l

append (x:xs) acc = x:append xs acc
append [] acc = acc

concat' xxs = foldr append [] xxs where

-- map' _ [] = []
-- map' f (x:xs) = f x : map' f xs

map' f xs = foldr (f . (:)) [] xs

cmap :: (a -> [b]) -> [a] -> [b]
cmap f xs = foldr (append . f) [] xs where
  append (x:xs) acc = x:append xs acc
  append [] acc = acc


--cmap f xs = foldr (\x acc -> map f x ++ acc) [] xs

subheads :: Tree a -> [a]
subheads (Tree.Node root subs) = map (\(Tree.Node a _) -> a) subs

heads :: Tree a -> a
heads (Tree.Node root _) = root

-- ignore subtrees, compare only nodes
--newtype CostsNode a = CN { unCN :: Tree (Costs a) } deriving Show

data Cursor a
  = CN { unCN :: (Tree (Costs a)), cnTrace :: [Costs a] }
  deriving Show

instance (Show a, Eq a) => Eq (Cursor a) where
  (==) (CN (Tree.Node a _) _) (CN (Tree.Node b _) _) = a == b

instance (Show a, Eq a) => Ord (Cursor a) where
  compare (CN (Tree.Node a _) _) (CN (Tree.Node b _) _) = compare a b

cnForest :: Tree (Costs a) -> Forest (Costs a)
cnForest x = [x]

upforest :: (Show a, Eq a) => [Costs a] -> Pq (Cursor a) -> Forest (Costs a) -> Pq (Cursor a)
upforest trace pq = foldr pqinsert pq . map (flip CN trace)


dfa :: forall a. (Eq a, Show a)
    => Pq (Cursor a) -> Cursor a -> Pq (Cursor a)
dfa pq0 node@(CN (Tree.Node _ []) _) =

  pqinsert node pq0
dfa pq0 node@(CN (Tree.Node t subs) trace) =

  pqmergeAll pq (map (dfa pqempty) (unPq pq))
  where
    pq = upforest (t:trace) pq0 subs
    zzz = 1

tt = root A [root B [leaf E], root G [leaf K]]


findTarget :: Node -> Pq (Cursor Node) -> Maybe (Cursor Node)
findTarget n = pqfind (\(CN (Tree.Node (Costs _ x) _) _) -> x == n)

scanTarget :: Node -> Pq (Cursor Node) -> (Maybe (Cursor Node), [Cursor Node])
scanTarget n pq
  = stopfoldr (fp (\(CN (Tree.Node (Costs _ x) _) _) -> x == n)) (Nothing, []) (PM.toAscList pq)

cursorPath :: Cursor t -> [Costs t]
cursorPath (CN (Tree.Node x _) trace) = x:trace

bg t = map cursorPath $ snd  $ scanTarget N $ dfa pqempty (CN t [])

--fscan :: (a -> Bool) -> [a] -> Maybe a
-- fscan pred xs = f `:`

-- | a foldr that can stop on (isJust.fst)
stopfoldr :: (a -> (Maybe a, [b]) -> (Maybe a, [b]))
       -> (Maybe a, [b])
       -> [a]
       -> (Maybe a, [b])
stopfoldr f a [] = a
stopfoldr f a@(Just _, _) _ = a
stopfoldr f a (x:xs) = f x (stopfoldr f a xs)

fp pred x (h, hist) = if pred x then (Just x, hist) else (h, x:hist)



  -- reachablePq :: Node -> Node -> Pq (Tree (Node, Int))
-- reachablePq to from =

minitree' = Tree.Node {Tree.rootLabel = Costs 0 J, Tree.subForest = [Tree.Node {Tree.rootLabel = Costs 2 L, Tree.subForest = [Tree.Node {Tree.rootLabel = Costs 10 M, Tree.subForest = [Tree.Node {Tree.rootLabel = Costs 13 N, Tree.subForest = []}]},Tree.Node {Tree.rootLabel = Costs 9 N, Tree.subForest = []}]},Tree.Node {Tree.rootLabel = Costs 6 I, Tree.subForest = [Tree.Node {Tree.rootLabel = Costs 8 L, Tree.subForest = [Tree.Node {Tree.rootLabel = Costs 16 M, Tree.subForest = [Tree.Node {Tree.rootLabel = Costs 19 N, Tree.subForest = []}]},Tree.Node {Tree.rootLabel = Costs 15 N, Tree.subForest = []}]},Tree.Node {Tree.rootLabel = Costs 9 M, Tree.subForest = [Tree.Node {Tree.rootLabel = Costs 12 N, Tree.subForest = []}]}]},Tree.Node {Tree.rootLabel = Costs 7 M, Tree.subForest = [Tree.Node {Tree.rootLabel = Costs 10 N, Tree.subForest = []}]}]}

minitree = bruteforce N J 0
bigtree = bruteforce N A 0

-- find (\(Costs _ n, _) -> n == N) $ depthFirst minitree
-- findTarget N $ dfa pqempty (CN minitree [])

idtree :: Tree a -> Tree a
idtree = foldTree root

-- idea: track a priority queue of subtrees we still want to consider
-- a priority queue should start with reachable paths and keep total costs


phtree = putStrLn . drawTree . fmap show
pvtree = putStrLn . drawVerticalTree . fmap show
  

--repl = anypath N A
