
discard :: (a -> Bool) -> [a] -> [a]
discard p xs = [x | x <- xs, not(p x)]

keep :: (a -> Bool) -> [a] -> [a]
keep p xs = [x | x <- xs, p x]


all
https://hoogle.haskell.org/?q=all