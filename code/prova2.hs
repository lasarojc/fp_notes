import Data.Char (toLower,isLetter)

import Data.Foldable as F



{-1
Um palíndromo é uma sequência que é igual ao seu reverso.
Não considere caracteres maiúsculos e minúsculos como iguais.
Defina uma função que dado uma String, verifique se ela é um palíndromo.

>>> éPalíndromo [1,2,3,2,1]
True


>>> éPalíndromo "abba"
True

>>> éPalíndromo "abBA"
False

>>> éPalíndromo [1.0,1.0,2.0]
False
-}

éPalíndromo :: Eq a => [a] -> Bool
éPalíndromo s = s == reverse s


{-2
Um pangrama é uma **frase** que contem todas as letras do alfabeto, ignorando-se diferenças entre maiúsculas e minúsculas.
Escreva uma função que, dado uma String, verifique se é um pangrama.

>>>éPangrama "Bancos fúteis pagavam-lhe queijo, whisky e xadrez"
True

>>>éPangrama "Já fiz vinho com toque de kiwi para belga sexy"
True

>>>éPangrama "The quick brown fox jumps over the lazy dog"
True

>>>éPangrama "Huguinho, Zézinho e Luisinho"
False
-}

éPangrama :: String -> Bool
éPangrama text = all (`elem` map toLower (filter isLetter text)) ['a'..'z']



{-
3 Defina um tipo algébrico para representar árvores binárias de qualquer tipo de dados.

4 Defina uma função que calcule a profundidade de uma árvore, onde profundidade é a maior distância entre a raiz e alguma folha da árvore.
Por exemplo, a profundidade da seguinte árvore é 3, devido ao comprimento do caminho a-c-e-g na árvore.
  a
 /  \
b    c
|    | \
d    e  f
     |
     g

a -+- b -+- d
   | 
   +- c -+- e -+- g
         |
         +- f

5 Defina uma função que retorne uma lista com os valores nas folhas de uma árvore, da esquerda para a direita. Para a árvore anterior, o resultado é [d,g,f]

6 Usando uma função fold, gere uma representação textual da sua escolha da árvore. Por exemplo

a (b (d) ()) (c (e (g) ()) (f))

>>>arv = Nó 'a'  (Nó 'b' (Nó 'd' Nada Nada) Nada) (Nó 'c' (Nó 'e' (Nó 'g' Nada Nada) Nada) (Nó 'f' Nada Nada))

>>>profundidade arv
3

>>>folhas arv
"dgf"

>>>textual arv
"'a' ('b' ('d' () ()) ()) ('c' ('e' ('g' () ()) ()) ('f' () ()))"

>>>textualF arv
"'a''b''d''c''e''g''f'"

-}

data Árvore a = Nada | Nó a (Árvore a) (Árvore a)
    deriving (Show)

instance F.Foldable Árvore where
    foldr f acc  Nada        = acc
    foldr f acc (Nó a ae ad) = f a (foldr f (foldr f acc ad) ae)

profundidade :: Árvore a -> Int
-- profundidade a = undefined
profundidade Nada = error "Não é uma árvore válida"
profundidade (Nó _ Nada Nada) = 0
profundidade (Nó _ Nada ad) = profundidade ad + 1
profundidade (Nó _ ae Nada) = profundidade ae + 1
profundidade (Nó _ ae ad) = max (profundidade ae) (profundidade ad) + 1


folhas :: Árvore a -> [a]
folhas Nada = []
folhas (Nó d Nada Nada) = [d]
folhas (Nó _ ae ad) = folhas ae ++ folhas ad


textual :: Show a => Árvore a -> String
textual Nada = ""
textual (Nó d ae ad) = show d ++ " (" ++ textual ae ++ ") (" ++ textual ad ++ ")"

textualF :: Show a => Árvore a -> String
textualF arv = foldr op "" arv
    where op :: Show a =>  a -> String -> String
          op d  y = show d ++ y

--textualF arv = foldr (\x y -> (case x of (Nó d _ _) -> show d 
--                                         _          -> "()"  ) ++ y) "" arv


{-
7 aplicação é uma função de ordem superior que recebe uma função f e um número n como parâmetros e retorna (f n).

>>>aplicação (^2) 3
9

-}

aplicação :: (a -> b) -> a -> b
aplicação f a = f a

{-
8 2XAplicação é uma função de ordem superior que recebe uma função f e um número n como parâmetros e retorna (f (f n)).

>>>x2Aplicação (^2) 3
81
-}

x2Aplicação :: (a -> a) -> a -> a
x2Aplicação f a = f $ f a


{-
9 xXAplicação é uma função de ordem superior que recebe uma função f, um número n e um número x como parâmetros e retorna (f (f ... (f n))), onde f aparece x vezes.

>>>xXAplicação (+1) 3 4
7
-}

xXAplicação :: (a -> a) -> a -> Int -> a
xXAplicação f p 0 = error "Contador não pode ser 0"
xXAplicação f p 1 = f p
xXAplicação f p c = f $ xXAplicação f p (c-1)


{-
10 Escreva uma função recursiva que calcule a média dos valores em uma lista.
11 Escreva uma função que calcule as médias das listas em uma lista, usando map.

>>>médiaLista [1,2,3]
2.0

>>>médiaListas [[1,2,3],[4,5,6]]
[2.0,5.0]
-}
médiaLista :: Fractional a => [a] -> a
médiaLista []     = error "Lista vazia"
médiaLista l      = médiaLista' l 0 0
    where médiaLista' []      sum count = sum/count
          médiaLista' (n:ns)  sum count = médiaLista' ns (sum + n) (count+1)

médiaListas :: Fractional a => [[a]] -> [a]
médiaListas = map médiaLista

{-
12 Escreva uma função que receba uma lista e retorne uma lista de tuplas em que os primeiros elementos são os elementos da lista e os segundos elementos são as quantidades de vezes que os elementos aparecem na lista.

>>> contaOcorrências [1,2,3,4,1,2,3,1,2] 2
(2,3)

>>> contaOcorrênciasTodos [1,2,3,4,1,2,3,1,2]
[(4,1),(3,2),(1,3),(2,3)]
-}

contaOcorrências :: Eq a => [a] -> a -> (a,Int)
contaOcorrências l e = (e, length (filter (== e) l))

contaOcorrênciasTodos :: Eq a => [a] -> [(a,Int)]
contaOcorrênciasTodos l = map (contaOcorrências l) únicos
    where únicos = foldr (\e es -> if e `elem` es then es else e:es) [] l


{-
13 Uma árvore de decisão é uma árvore binária em que o nó tem um critério de decisão D, o filho da esquerda contém uma subárvore em que D é satisfeito e o filho da direita uma subárvore em que D não é satisfeito. Por exemplo

                  cansado
                /        \
             Sim          Não
            /                \
        É noite              Trabalhando
       /       \                 \      \
    Sim        Não               Sim     Nao
    /            \                |        \
Dormir         Trabalhando     Promoção   Vá correr
              /           \
           Sim            Não
           /                \
        Complicou         Cochilar

Escreva uma função que receba uma árvore de decisão e uma lista de respostas e retorne a folha correspondente da árvore.
No exemplo anterior, [Sim,Não,Não] retorna o resultado "Cochilar"

>>> arvD = Nó "cansado" (Nó "É noite" (Nó "Dormir" Nada Nada) (Nó "Trabalhando" (Nó "Complicou" Nada Nada) (Nó "Cochilar" Nada Nada))) (Nó "Trabalhando" (Nó "Promoção" Nada Nada) (Nó "Vá correr" Nada Nada))

>>> print arvD

>>>percorrer [Sim,Não,Não] arvD
"Cochilar"
 -}

data Resposta = Sim | Não

percorrer :: [Resposta] -> Árvore String -> String
percorrer _ Nada                 = "Nada"
percorrer [] (Nó a _ _ )         = a
percorrer (Sim:xs) (Nó _ ae _ )  = percorrer xs ae
percorrer (Não:xs) (Nó _ _ ad )  = percorrer xs ad