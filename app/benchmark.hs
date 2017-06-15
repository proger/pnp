module Main where

import Criterion.Main
import Data.List (sort)
import System.Random

numbers :: IO ([Int], [Int], [Int])
numbers = do
  g <- getStdGen
  let xs = take 50 (randoms g :: [Int])
  let sorted    = sort xs
      revsorted = reverse sorted
  return (xs, sorted, revsorted)

sorts :: Benchmark
sorts = env numbers $ \ ~(random,sorted,revsorted) ->
  bgroup "sort"
  [ bgroup "base"
    [ bench "sorted"    $ nf sort sorted
    , bench "random"    $ nf sort random
    , bench "revsorted" $ nf sort revsorted
    ]
  ]

main :: IO ()
main = defaultMain [ sorts ]
