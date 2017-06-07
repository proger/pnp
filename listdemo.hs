

data List a = Cons a (List a) | Nil deriving Show

repl = foldr (+) 0 (1:2:3:4:5:[])
