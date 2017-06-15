

data X = A | B | C | D | E | F | G | H | I deriving Show

-- [1,2,3] -> [[],[1],[1,2],[1,2,3],[2],[2,3],[3]]

pairs  (h:t) = [[h,x] | x <- t] ++ pairs t
pairs [] = []

maptails :: (t -> [t] -> a) -> [t] -> [a]
maptails f (x:xs) = (f x xs):(maptails f xs)
maptails f [] = []

ps :: [t] -> [t] -> [[t]]
ps p xs = [p++[x] | x <- xs] ++ concat (maptails (\h t -> ps (p++[h]) t) xs)

powerset :: [t] -> [[t]]
powerset = ([]:) . ps []

powerset' (x:xs) = powerset' xs ++ map (x:) (powerset' xs)
powerset' [] = [[]]


fac n = product [1..n]

choose n k = fac n `div` (fac k * fac (n-k))

psize n = sum [choose n k | k <- [0..n]]

{-
[1,2,3,4] -> [0, 1, 12, 123, 1234,
                        124,
                    13, 134,
                    14,
                 2, 23, 234,
                    24,
                 3, 34,
                 4]

[1,2,3,4] -> [0, 1, 2, 3, 4, 12, 13, 14, 23, 24, 34, 123, 124, 134, 234, 1234]
-}
