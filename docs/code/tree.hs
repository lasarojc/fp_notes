data Árvore a = Nada | Nó a (Árvore a) (Árvore a)  deriving (Show)

{-
>>> mudinha = Nó 3 Nada Nada

>>> Nó 1 (Nó 2 (Nó 4 Nada Nada) Nada) (Nó 3 Nada Nada)
Nó 1 (Nó 2 (Nó 4 Nada Nada) Nada) (Nó 3 Nada Nada)
-}

adicionar :: (Eq a, Ord a) => a -> Árvore a -> Árvore a
adicionar novoDado Nada = Nó novoDado Nada Nada
adicionar novoDado árvore@(Nó dadoExistente ae ad)
    | novoDado == dadoExistente = árvore
    | novoDado < dadoExistente  = Nó dadoExistente (adicionar novoDado ae) ad
    | otherwise                 = Nó dadoExistente ad (adicionar novoDado ad)

{-
>>> arv = adicionar 3 (adicionar 4 (adicionar 10 (adicionar 1 (adicionar 2 (adicionar 3 (adicionar 7 Nada))))))

>>> arv
Nó 7 (Nó 4 (Nó 3 Nada Nada) Nada) (Nó 10 Nada Nada)

>>> impressãoEmOrdem arv
".3.4.7.10."
-}

impressãoEmOrdem :: (Show a) => Árvore a -> String
impressãoEmOrdem Nada = "."
impressãoEmOrdem (Nó dado ae ad) = impressãoEmOrdem ae ++ show dado ++ impressãoEmOrdem ad


