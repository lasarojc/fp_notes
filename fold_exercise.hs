{- Inspirado em https://www.cantab.net/users/antoni.diller/haskell/questions/quest06.pdf

1 - Usando foldr, defina uma função somaQuads que recebe como parâmetro um inteiro n e retorna a soma dos quadrados dos primeiros n inteiros. somaQuads n = 1^2 + 2^2 + 3^2 ... n^2

>>> somaQuads 3
15

>>> somaQuads' 3
15

>>> somaQuadsL 3
15


2 - Usando fold, defina uma função que calcule o comprimento de uma lista.

>>>comp [1..10]
10

3 - Usando foldr, defina uma função que calcule o menor número em uma lista não vazia de inteiros.
>>> mínimo [10,9..0]
0
>>> mínimo [10..100]
10


4 - Usando foldr, defina uam função que reverta uma lista.

>>> reverte [1..10]
[10,9,8,7,6,5,4,3,2,1]

5 - Usando foldr, defina uma função que recebe duas listas e que remova todo elemento da segunda lista que aparece na primeira lista.

6 - Defina a função filter' usando foldr.

Refaça todos os exercícios usando foldl.
-}

















somaQuads n = foldr ((+).(^2)) 0 [1..n]

somaQuad x y = x*x + y

somaQuads' n = foldr somaQuad 0 [1..n]

somaQuadL x y = y*y + x

somaQuadsL n = foldl somaQuadL 0 [1..n]




-- >>>comp [1..10]
-- 10


count _ y = 1 + y
comp l = foldr count 0 l

-- >>>comp' [1..10]
-- 10

comp' l = foldr (\_ y -> 1+y) 0 l


menor x y = if x < y then x else y

-- >>> mínimo [1..10]
-- 1

-- >>> mínimo [10,9..1]
-- 1

mínimo [] = error "Impossível"
mínimo l = foldr menor (maxBound :: Int) l



-- >>> reverte [1..10]
-- [10,9,8,7,6,5,4,3,2,1]
reverte l = foldr (\x y -> y ++ [x]) [] [1..10]


-- >>> removeDup [1,3..10] [1..10]
-- [2,4,6,8,10]

removeSePresente e = filter (/=e)

removeDup l1 l2 = foldr removeSePresente l2 l1

-- >>> removeDup' [1,3..10] [1..10]
-- [2,4,6,8,10]

removeDup' l1 l2 = foldr (\e l -> filter (/=e) l) l2 l1
