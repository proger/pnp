
data Source = A | B | C | D deriving (Show, Enum, Bounded, Eq)

data Target = X | Y | Z | W deriving (Show, Enum, Bounded, Eq)

enum :: (Bounded a, Enum a) => [a]
enum = [minBound..maxBound]

src s = case s of
  A -> 3
  B -> 9
  C -> 6
  D -> 4

tar t = case t of
  X -> 5
  Y -> 6
  Z -> 6
  W -> 5

-- mapM_ print $ pure (,) <*> (enum :: [Source]) <*> (enum :: [Target])
cost :: Source -> Target -> Int
cost src tar = case (src, tar) of
  (A,X) -> 2
  (A,Y) -> 3
  (A,Z) -> 5
  (A,W) -> 7
  (B,X) -> 7
  (B,Y) -> 8
  (B,Z) -> 2
  (B,W) -> 5
  (C,X) -> 4
  (C,Y) -> 6
  (C,Z) -> 8
  (C,W) -> 7
  (D,X) -> 7
  (D,Y) -> 4
  (D,Z) -> 8
  (D,W) -> 2


newtype Transfer from to = Transfer Int
