module Optimize.Tree where

import Data.Tree

phtree :: Show a => Tree a -> IO ()
phtree = putStrLn . drawTree . fmap show

root = Node

leaf = flip root []

foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree f = go where
  go (Node x ts) = f x (map go ts)

-- | returns paths from leaves in depth-first manner
depthFirst :: Tree a -> [[a]]
depthFirst = depthFirst' []

depthFirst' :: [a] -> Tree a -> [[a]]
depthFirst' trace (Node root []) =
  [(root:trace)]
depthFirst' trace (Node root subs) =
  foldr (\subtree acc -> (depthFirst' (root:trace) subtree) ++ acc) [] subs


-- bestFirstDepth = bfd []

-- bfd :: [a] -> Tree a -> [[a]]
-- bfd trace (Node root []) =
--   [(root:trace)]
-- depthFirst' trace (Node root subs) =
--   foldr (\subtree acc -> (depthFirst' (root:trace) subtree) ++ acc) [] subs


-- pvtree = putStrLn . drawVerticalTree . fmap show
