import Data.Foldable as F

data Árvore a = Nada | Nó a (Árvore a) (Árvore a)
    deriving (Show,Eq,Read)

instance F.Foldable Árvore where
    foldMap f Nada         = mempty
    foldMap f (Nó a ae ad) = F.foldMap f ae   `mappend` 
                             f a              `mappend` 
                             F.foldMap f ad

    foldr f acc  Nada        = acc
    foldr f acc (Nó a ae ad) = foldr f (f a (foldr f acc ad)) ae

{- 
>>>arv = Nó 1    (Nó 2 (Nó 4 Nada Nada) Nada)     (Nó 3 Nada Nada)

>>> foldr (+) 0 arv
10

>>>arv2 = Nó  "a"    (Nó "b" (Nó "c" Nada Nada) Nada)     (Nó "d" Nada Nada)

>>> foldr (++)  "" arv2
"cbad"
-}
