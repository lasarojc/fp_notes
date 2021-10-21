import Debug.Trace(trace)
import Data.List (delete,isPrefixOf)
import Data.Char (ord)


{- 
>>>fat 0
1

>>>fat 5
120

>>>fat (-2)
Indefinido

>>>show [1..(-3)]
"[]"

>>>fat 5
30414093201713378043612608166064768844377641568960512000000000000
-}

fat 0 = 1
fat n
    |n > 0 = n * fat (n-1)
fat _ = error "Indefinido"



{-
>>>diasMes 5
31

>>>diasMes 5.1
No instance for (Fractional Int) arising from the literal ‘5.1’

>>>diasMes 37::Int
30

>>>diasMes "lala"
Couldn't match expected type ‘Int’ with actual type ‘[Char]’
-}

diasMes :: Int -> Int
diasMes m
    | m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12 = 31
    | m == 2 = 28
    | otherwise = 30




{-
>>>minhaFunção "entrada1" 2 2
"tr"
>>>minhaFunção "entrada1" 2 3
"tra"
-}

minhaFunção :: String -> Int -> Int -> String
minhaFunção s t n = reverse (take n (reverse (take (t + n) s)))



{-
>>>éNeg' (-3)
(True,3)

>>>éNeg' 3
(False,3)

-}

éNeg' :: Int -> (Bool,Int)
éNeg' x = (x < 0, abs x)






testaNegativo :: Int -> (Bool, Int)
testaNegativo i = (i < 0, abs i)



type Inteiro = Int

somaInteiros :: Inteiro -> Inteiro -> Inteiro
somaInteiros a b = a + b


minhaFunção' 10 = "Deu certo"

minhaFunção'' (a,b) ((c,d),_,f) g = a + b + c + d + f + fst g + snd g


próximos3 :: Int -> Char -> (Int,Int,Int)
próximos3 0 _ = (-1,0,1)
próximos3 n 'd'
    | n > 0 = (n-1,n-2,n-3)
    | n < 0 = (n+1,n+2,n+3)

próximos3 n 'a'
    | n < 0 = (n-1,n-2,n-3)
    | n > 0 = (n+1,n+2,n+3)

próximos3 n _ = error "Use d ou a"

fatorial' 0 = 1
fatorial' n
    |n > 0 = n * fatorial' (n-1)
fatorial' n
    |n < 0 = error "Indefinido"


{-
>>>nomeMes 10
"OUT"

>>>nomeMes 13
Nao deu

-}

nomeMes 1 = "JAN"
nomeMes 2 = "FEB"
nomeMes 3 = "ABR"
nomeMes 4 = "MAR"
nomeMes 5 = "MAI"
nomeMes 6 = "JUN"
nomeMes 7 = "JUL"
nomeMes 8 = "AGO"
nomeMes 9 = "SET"
nomeMes 10 = "OUT"
nomeMes 11 = "NOV"
nomeMes 12 = "DEZ"
nomeMes x = error "Nao deu"



{-
>>>mult 0 1
0

>>>mult 1 0
0

>>>mult 1 2
2

>>>mult 'c' 3
Couldn't match expected type ‘Int’ with actual type ‘Char’
-}

mult :: Int -> Int -> Int
mult _ 0 = 0
mult 0 _ = 0
mult x y = x * y



not :: Bool -> Bool
not True = False
not _ = True


e :: Bool -> Bool -> Bool
e True True = True
e _ _ = False



type Nome = (String, String, String)
type Telefone = (String, String)
type CPF = String
type Endereço = (String, String, String)
type Pessoa = (Nome, Telefone, CPF, Endereço)

fazPessoa :: Nome -> Telefone -> CPF -> Endereço -> Pessoa -- (1) Definição da função.
fazPessoa nome telefone cpf endereço = (nome, telefone, cpf, endereço)

pegaNome :: Pessoa -> Nome
pegaNome (nome, _, _, _) = nome

pegaTelefone :: Pessoa -> Telefone
pegaTelefone (_, telefone, _, _) = telefone

trocaTelefone :: Pessoa -> Telefone -> Pessoa
trocaTelefone (n, _t, c, e) novoTelefone = (n, novoTelefone, c, e)

pegaSobreNomeDeNome :: Nome -> String
pegaSobreNomeDeNome (primeiro,segundo,sobre) = sobre

{-
>>>pegaSobrenome (fazPessoa ("Joao","Jose", "Maria") ("lala","llala") "CPF" ("lala", "lele", "ili"))
"Maria"

>>>pegaSobreNome (fazPessoa ("Joao","Jose", "Maria") ("lala","llala") "CPF" ("lala", "lele", "ili"))
"Maria"
 -}

pegaSobrenome :: Pessoa -> String
pegaSobrenome p = pegaSobreNomeDeNome (pegaNome p)


pegaSobreNome :: Pessoa -> String
pegaSobreNome ((_,_,sobre), _, _, _) = sobre


{-
>>>próximos3'' 0 'd'
(-1,0,1)

>>>próximos3' 0 'd'
(-1,0,1)

>>>próximos3'' 0 'x'
(-1,0,1)

>>>próximos3' 0 'x'
/Users/lasarocamargos/ufu/fp_notes/docs/code/scratch.hs:(234,24)-(237,107): Non-exhaustive patterns in case

-}


próximos3'' :: Int -> Char -> (Int,Int,Int)
próximos3'' 0 _ = (-1,0,1)
próximos3'' n 'd'
    | n > 0 = (n-1,n-2,n-3)
    | n < 0 = (n+1,n+2,n+3)

próximos3'' n 'a'
    | n < 0 = (n-1,n-2,n-3)
    | n > 0 = (n+1,n+2,n+3)

próximos3'' _ _ = error "Use d ou a"


próximos3' :: Int -> Char -> (Int,Int,Int)
próximos3' n direcao = case direcao of 'd' -> case n of 0 -> (-1,0,1)
                                                        _ -> if n > 0 then (n-1,n-2,n-3) else (n+1,n+2,n+3)
                                       'a' -> case n of 0 -> (-1,0,1)
                                                        _ -> if n < 0 then (n-1,n-2,n-3) else (n+1,n+2,n+3)


próximos3''' :: Int -> Char -> (Int,Int,Int)
próximos3''' n dir = case n of 0 -> (-1,0,1)
                               _ -> case dir of 'd' -> if n > 0 then (n-1,n-2,n-3) else (n+1,n+2,n+3)
                                                'a' -> if n < 0 then (n-1,n-2,n-3) else (n+1,n+2,n+3)
                                                _   -> error "Use d ou a"



{-
Assim como Bool pode assumir os valores True e False, o tipo Naipe pode assumir um dos valores Copas, Espada, Ouro e Paus, tal que
Copas < Espada < Ouro < Paus

>>>Copas > Espada
False

>>> Espada > Ouro
False

>>> Paus > Copas
True
-}


data Naipe = Copas | Espada | Ouro | Paus --deriving (Ord,Eq,Enum,Show)




fatorialPM 0 = 1
fatorialPM n = n * fatorialPM (n-1)

fatorialGuardas n
     | n == 0 = 1
     | n > 0 = n * fatorialGuardas (n-1)

fatorialGuardas' n
     | n == 0 = 1
     | n > 0 = n * fatorialGuardas' (n-1)
     | otherwise = error "Não se pode calcular o fatorial de números negativos"

fatorialGuardas'' n
     | n == 0 = 1
     | otherwise = if n > 0 then n * fatorialGuardas'' (n-1)
                            else error "Não se pode calcular o fatorial de números negativos"



mdc :: Integer -> Integer -> Integer
mdc a b | b == 0     = a
        | otherwise  = mdc b (a `mod` b)

mdc' :: Integer -> Integer -> Integer
mdc' a 0 = a
mdc' a b = mdc' b (a `mod` b)

mdc'' :: Integral t => t -> t -> t
mdc'' a b = case b of 0 -> a
                      _ -> mdc'' b (a `mod` b)

mdc''' :: Integral t => t -> t -> t
mdc''' a b = if b == 0 then a
                       else mdc''' b (a `mod` b)




{-
- entrada: três números inteiros de precisão finita (Int)
- saída: Int
-}
somaTresNumeros :: Int -> Int -> Int -> Int
somaTresNumeros n1 n2 n3 = n1 + n2 + n3

{-
Esta função retorna a área do quadrado de lado l

areaQuadrado
- Entrada: l, Float
- Saída: l**2, Float
-}
areaQuadrado :: Float -> Float
areaQuadrado l = l**2

{-
Esta função retorna a área do retângulo de lado l1 e l2

areaRetangulo
- Entrada: l1 e l2, Floats
- Saída: l1 * l2, Float
-}
areaRetangulo :: Float -> Float -> Float
areaRetangulo l1 l2 = l1 * l2


-- Defina as seguintes funções usando if then else
{-
Esta função retorna o maior de tres números

maiorDeTres
- Entrada: a b c, Integer
- Saída: o maior dentre a b c, Integer
-}
maiorDeTres :: Integer -> Integer -> Integer -> Integer
maiorDeTres a b c = if a >= b && a >= c then a
                    else if b >= c then b
                    else c


-- Defina as seguintes funções, incluindo a definição de tipos.

{-
Esta função calcula quem ganha no jogo pedra/tesoura/papel

pedraTesouraPapel
- Entrada: m1 m2 - "pedra" "tesoura" "papel"
- Saída: O valor ganhador, ou "empate" no caso empate

>>>pedraTesouraPapel "pedra" "papel"
"papel"

>>>pedraTesouraPapel "pedra" "tesoura"
"pedra"

>>>pedraTesouraPapel "papel" "tesoura"
"tesoura"

>>>pedraTesouraPapel "tesoura" "tesoura"
"empate"
-}
pedraTesouraPapel :: String -> String -> String
pedraTesouraPapel m1 m2
                        |m1 == m2 = "empate"
                        |m1 == "pedra" = if m2 == "tesoura" then "pedra" else "papel"
                        |m1 == "papel" = if m2 == "tesoura" then "tesoura" else "papel"
                        |m1 == "tesoura" = if m2 == "papel" then "tesoura" else "pedra"
                        |otherwise = error "Erro!"

{-
Esta função calcula a conjunção do dois parâmetros (e lógico).

>>>eLógico True True
True

>>>eLógico True False
False

>>>eLógico False True
False

>>>eLógico False False
False
-}
eLógico :: Bool -> Bool -> Bool
eLógico p1 p2
             | p1 == True && p2 == True = True
             |otherwise = False

{-
Esta função calcula a disjunção do dois parâmetros (ou lógico).

>>>ouLógico True True
True

>>>ouLógico True False
True

>>>ouLógico False True
True

>>>ouLógico False False
False
-}
ouLógico :: Bool -> Bool -> Bool
ouLógico p1 p2
             | p1 == False && p2 == False = False
             |otherwise = True


-- Defina as seguintes funções usando ++ !! take e reverse.

{-
Defina função que retorne substring de t elementos começando na posição i
subStringDeAte "entrada1" 2 2 retorna "tr"

Entrada:
    - s: string
    - i: inteiro
    - t: inteiro
    
>>>subStringDeAte "entrada1" 2 2
"tr"
-}
subStringDeAte :: String -> Int -> Int -> String
subStringDeAte s i t = take t (drop i s)

{-
Defina função que retorne substring com os últimos u elementos

Entrada:
    - s: string
    - u: inteiro

>>>últimosU "entrada1" 3
"da1"
-}
últimosU :: String -> Int -> String
últimosU s u =  drop ((length s) - u) s

{-
Defina função que receba duas strings e retorne a resultado da concatenação das substrings de t elementos começando na posição i

Entrada
    - s1: string
    - s2: string
    - i: inteiro
    - u: inteiro
    
>>>subStringDeAteAppend "entrada1" "entrada2" 2 2
"trtr"
-}

subStringDeAteAppend s1 s2 i t = take t (drop i s1) ++ take t (drop i s2)




-- Nas próximas funções, trabalharemos com definições de tipo e tuplas.

{-
Seja o tipo de dados Carta tupla em que 
 - o primeiro elemento é o valor da carta (1,2,3,4,5,6,7,8,9,10,11,12,13) 
 - o segundo é o naipe ("ouro", "copas", "espada", "paus").
-}
type Carta = (Int, String)

{-
Uma função que receba uma carta retorne seu naipe.

Entrada:
    - c1: carta

Resultado: naipe da carta

>>>naipe (13, "paus")
"paus"

-}
naipe :: Carta -> String
naipe (_, np) = np

{-
Uma função que receba uma carta retorne seu valor.

Entrada:
    - c1: carta

Resultado: valor da carta

>>>valor (2, "ouro")
2
-}
valor :: Carta -> Int
valor (vl, _) = vl


{-
Uma função que receba duas cartas e diga se a primeira é menor que a segunda.
Uma carta c1 é menor que uma carta c2 se valor c1 < valor2 OU se valor c1 == valor c2 e naipe c1 < c2.
"copas" < "espada" < "ouro" < "paus"

Entrada:
    - c1, c2: Carta
Resultado: True ou False

>>>cartaÉMenor (3, "paus") (2, "ouro")
False

>>>cartaÉMenor (3, "ouro") (2, "copas")
False

>>>cartaÉMenor (3, "espada") (2, "copas")
False

>>>cartaÉMenor (1, "copas") (2, "espada")
True

>>>cartaÉMenor (1, "copas") (2, "copas")
True

>>>cartaÉMenor (2, "copas") (2, "copas")
False

>>>cartaÉMenor (3, "copas") (2, "copas")
False

>>>cartaÉMenor (3, "ouro") (3, "paus")
True

-}
cartaÉMenor :: Carta -> Carta -> Bool
cartaÉMenor c1 c2
                | valor c1 < valor c2 = True
                | valor c1 > valor c2 = False
                | otherwise = if naipe c1 == naipe c2 then False
                            else if naipe c1 == "copas" then True
                            else if naipe c1 == "paus" then False
                            else if naipe c1 == "espada" && naipe c2 == "copas" then False
                            else naipe c1 == "ouro" && naipe c2 == "paus"

{-
Uma função que receba duas cartas e diga se a primeira é igual à segunda.

Entrada:
    - c1, c2: Carta
Resultado: True ou False

>>>cartaÉIgual (2, "copas") (2, "copas")
True

>>>cartaÉIgual (3, "copas") (2, "copas")
False

>>>cartaÉIgual (2, "copas") (2, "ouro")
True

-}
cartaÉIgual :: Carta -> Carta -> Bool
cartaÉIgual c1 c2 = valor c1 == valor c2 && naipe c1 == naipe c2

{-
Uma função que receba duas cartas e diga se a primeira é maior que a segunda.

Entrada:
    - c1, c2: Carta
Resultado: True ou False

-}
cartaÉMaior :: Carta -> Carta -> Bool
cartaÉMaior c1 c2
                | valor c1 > valor c2 = True
                | valor c1 < valor c2 = False
                | otherwise = if naipe c1 == naipe c2 then False
                            else if naipe c1 == "copas" then False
                            else if naipe c1 == "paus" then True
                            else if naipe c1 == "ouro" && naipe c2 == "paus" then False
                            else naipe c1 == "espada" && naipe c2 == "copas"


{-
Uma função que receba três cartas c1 c2 c3 e diga se formam um jogo.
- seja m1 a maior dentre as cartas c1 c2 c3
- seja m2 a de valor mediano dentre as cartas c1 c2 c3
- seja m3 a menor dentre as cartas c1 c2 c3
m1 m2 e m3 formam um jogo se e somente si
    - OU naipe m1 == naipe m2 == naipe m3 E valor m1 == valor m2 + 1 == valor m3 + 2
    - OU naipe m1 =/= naipe m2 =/= naipe m3 =/= m1 E valor m1 == valor m2 == valor m3


Entrada:
    - c1, c2, c3: Carta
Resultado: True ou False
-}
m1 :: Carta -> Carta -> Carta -> Carta
m1 c1 c2 c3
                | cartaÉMaior c1 c2 == True && cartaÉMaior c1 c3 == True = c1
                | cartaÉMaior c2 c1 == True && cartaÉMaior c2 c3 == True = c2
                | otherwise = c3

m2 :: Carta -> Carta -> Carta -> Carta
m2 c1 c2 c3
           | valor c1 < valor c2 && valor c1 > valor c3 = c1
           | valor c1 < valor c3 && valor c1 > valor c2 = c1
           | valor c2 < valor c1 && valor c2 > valor c3 = c2
           | valor c2 < valor c3 && valor c2 > valor c1 = c2
           | otherwise = c3

m3 :: Carta -> Carta -> Carta -> Carta
m3 c1 c2 c3
           | cartaÉMenor c1 c2 == True && cartaÉMenor c1 c3 == True = c1
           | cartaÉMenor c2 c1 == True && cartaÉMenor c2 c3 == True = c2
           | otherwise = c3


éJogo :: Carta -> Carta -> Carta -> Bool
éJogo c1 c2 c3
              | naipe (m1 c1 c2 c3) == naipe (m2 c1 c2 c3) &&
              naipe (m1 c1 c2 c3) == naipe (m3 c1 c2 c3) &&
              naipe (m2 c1 c2 c3) == naipe (m3 c1 c2 c3) &&
              valor (m1 c1 c2 c3) == valor (m2 c1 c2 c3) + 1 &&
              valor (m1 c1 c2 c3) == valor (m3 c1 c2 c3) + 2 &&
              valor (m2 c1 c2 c3) + 1 == valor (m3 c1 c2 c3) + 2 = True
              | naipe (m1 c1 c2 c3) /= naipe (m2 c1 c2 c3) &&
              naipe (m1 c1 c2 c3) /= naipe (m3 c1 c2 c3) &&
              naipe (m2 c1 c2 c3) /= naipe (m3 c1 c2 c3) &&
              valor (m1 c1 c2 c3) == valor (m2 c1 c2 c3) &&
              valor (m1 c1 c2 c3) == valor (m3 c1 c2 c3) &&
              valor (m2 c1 c2 c3) == valor (m3 c1 c2 c3) = True
              | otherwise = False


{-
Defina uma função que receba duas tuplas de 3 cartas, onde a primeira carta é maior ou igual à segunda, que é maior ou igual à terceira, 
compare as tuplas, e retorne a maior dentre as duas mãos.
Uma tupla t1 é menor que uma tupla t2 se e somente se
- a primeira carta de t1 é menor que a primeira de t2 OU
- as primeiras cartas são iguais mas a segunda carta de t1 é menor que a segunda de t2
- as primeiras e segundas cartas são iguais mas a terceira carta de t1 é menor que a terceira de t2
-}
maiorMão :: (Carta, Carta, Carta) -> (Carta, Carta, Carta) -> (Carta, Carta, Carta)
maiorMão (c1, c2, c3) (c4, c5, c6)
            | cartaÉMaior c4 c1 == True = (c4, c5, c6)
            | cartaÉIgual c1 c4 == True && cartaÉMaior c5 c2 == True = (c4, c5, c6)
            | cartaÉIgual c1 c4 == True && cartaÉIgual c2 c5 == True && cartaÉMaior c6 c3 == True = (c4, c5, c6)
            | otherwise = (c1, c2, c3)
{-
Defina uma função que receba duas tuplas de 3 cartas, onde a primeira carta é maior ou igual à segunda, que é maior ou igual à terceira, 
e compare as tuplas para dizer qual ganha, ou se houve empate.
Uma tupla t1 ganha de uma tupla t2 se 
- t1 é um jogo mas t2 não é um jogo
- t1 e t2 são um jogo mas t1 é maior que t2.
- se nenhuma é jogo e t1 é maior que t2
- No caso de empate, retorne a tupla ((0,""),(0,""),(0,""))
-}

mãoGanhadora :: (Carta, Carta, Carta) -> (Carta, Carta, Carta) -> (Carta, Carta, Carta)
mãoGanhadora (c1, c2, c3) (c4, c5, c6)
            | cartaÉIgual c1 c4 == True && cartaÉIgual c2 c5 == True && cartaÉIgual c3 c6 == True = ((0, ""), (0, ""), (0, ""))
            | éJogo c1 c2 c3 == True && éJogo c4 c5 c6 == False = (c1, c2, c3)
            | éJogo c1 c2 c3 == True && éJogo c4 c5 c6 == True && maiorMão (c1, c2, c3) (c4, c5, c6) == (c1, c2, c3) = (c1, c2, c3)
            | éJogo c1 c2 c3 == False && éJogo c4 c5 c6 == False && maiorMão (c1, c2, c3) (c4, c5, c6) == (c1, c2, c3) = (c1, c2, c3)
            | otherwise = (c4, c5, c6)



{-
>>>imc 70 1.8
-}
imc :: Double -> Double -> String
imc p a
    | imc' <= baixo = "Baixo"
    | imc' <= normal = "Normal"
    | imc' <= alto = "Alto"
    where imc' = trace "hmmm... " (p / a ^ 2)
          baixo = trace "b" (18.5)
          normal = trace "n" (25.0)
          alto = trace "a" (30.0)


fib:: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = let prev     = fib (n - 1)
            prevPrev = fib (n - 2)
        in prev + prevPrev


vaiEVolta n = lista ++ drop 1 (reverse lista)
    where lista = [1..n]


éPalíndromo s = s == reverse s



imc'' p a
      | imcDeVerdade <= 18.5 = "Baixo"
      | imcDeVerdade <= 25.0 = "Normal"
      | imcDeVerdade <= 30.0 = "Alto"
      where imcDeVerdade = p / a^2


teste :: Int -> Int -> Double
teste x y =  xD / yD
    where xD = fromIntegral x ::Double
          yD = fromIntegral y ::Double


x = 0
soma y
 | y == 0 = 0
 | y > 0 = y + soma(y-1)  -- .... queria tambem mudar o valor do meu x, fazer tipo um x += 2

{-
 soma 3 = 3 + soma 2
        = 3 + (2 + soma 1)
        = 3 + (2 + (1 + soma 0))
        = 3 + (2 + (1 + 0))
        = 3 + (2 + 1)
        = 3 + 3
        = 6
-}

{-
Defina uma função que arredonde um número Real pra um Integer de forma que se for maior que 0, arredonde para baixo e se for menor que zero arredonde para cima.
Dica: use floor e ceiling

>>>arredonda0 (-2.5)
-2
>>>arredonda0 (2.5)
2

Defina uma função que arredonde um número Real pra um Integer de forma que se for maior que 0, arredonde para cima e se for menor que zero arredonde para baixo

>>>arredondaInf (-1.5)
Variable not in scope: arredondaInf :: t0 -> t
>>>arredondaInf (1.5)
Variable not in scope: arredondaInf :: t0 -> t

-}

arredonda0 :: Float -> Int
arredonda0 x
    | menorQueZero = cima
    | maiorQueZero = baixo
    | otherwise = 0
    where cima = ceiling x
          baixo = floor x
          maiorQueZero = x > 0
          menorQueZero = x < 0

arredonda0let :: Float -> Int
arredonda0let x
    | menorQueZero = let cima = ceiling x in cima
    | maiorQueZero = let baixo = floor x in baixo
    | otherwise = 0
    where maiorQueZero = x > 0
          menorQueZero = x < 0

arredonda0let' :: Float -> Int
arredonda0let' x
    | x > 0  = let cima = ceiling x in cima
    | x <= 0 = let baixo = floor x in baixo
    | otherwise = 0


{- 
Reescreva a função acima usando let-in em vez de where
-}

arredonda :: Float -> Int
arredonda x = let maior = x >= 0
                  menor = x < 0
                  cima = ceiling x
                  baixo = floor x
              in if maior then baixo
                          else cima

arredonda0' :: Float -> Int
arredonda0' x = if x >= 0 then let cima = floor x in cima
                          else let baixo = ceiling x in baixo



teste' [] = []
teste' (x:y:z:es) = [x, y, z]
teste' (x:_) = [x]


oQueHáNaLista :: (Show a) => [a] -> String
oQueHáNaLista [] = "Nada"
oQueHáNaLista (x:[]) = "Só " ++ (show x)
oQueHáNaLista (x1:x2:[]) = "Há " ++ (show x1) ++ " e " ++ (show x2)
oQueHáNaLista (x:xs) = "Há " ++ (show x) ++ " e mais um monte de coisas"


iniciais :: [String] -> [Char]
iniciais [] = []
iniciais [(x:_)] = [x]
iniciais [(x:_),(y:_)] = [x,y]
iniciais ((x:_):(y:_):(z:_):_) = [x,y,z]




{-
>>>divideEmN 3 "lasfadafdsafsfdasdfasdfasdfasdf"
["las","fad","afd","saf","sfd","asd","fas","dfa","sdf","asd","f"]
-}

divideEmN :: Int -> String -> [String]
divideEmN n l = if null l
                then []
                else primeiraParte : divideEmN n segundaParte
        where (primeiraParte, segundaParte) = splitAt n l


divideEmC :: Char -> String -> [String]
divideEmC c l = if null l
                then []
                else primeiraParte : divideEmC c segundaParte
        where
            achaIndice = error "implementar"
            n = achaIndice c l
            (primeiraParte, segundaParte) = splitAt n l


{-
>>>divideEmC' 'x' "asdfasdfaxASDAFSDAF"

-}
divideEmC' :: Char -> String -> [String]
divideEmC' c l =
    let primeiraParte = takeWhile (/=c) l
        segundaParte = if null resultadoDoDrop then []
                                               else tail resultadoDoDrop
            where resultadoDoDrop = dropWhile (/=c) l
    in if null l
        then []
        else primeiraParte : divideEmC' c segundaParte

segundaParte c l = if null resultadoDoDrop then []
                                        else tail resultadoDoDrop
            where resultadoDoDrop = dropWhile (/=c) l


{-
>>>compacte [[1,1],[12],[2,2],[3,3,3],[4,4,4],[3,3,3]]
[(1,2),(12,1),(2,2),(3,3),(4,3),(3,3)]
-}

compacte :: [[a]] -> [(a, Int)]
compacte l = if null l then []
             else (head(head l), length (head l))  : compacte (tail l)





{-

(:) :: a -> [a] -> [a]
>>> 1 : []
[1]
>>> 1 : [2,3]
[1,2,3]

>>> 1 : 2 : 3 : 4:[]
[1,2,3,4]

>>>"aaaa" : "bbbb" : []
["aaaa","bbbb"]

>>>['a','a'] : "bbbb" : []
["aa","bbbb"]

>>>[1,2,3]: [4,5,6]: [7,8] : []
[[1,2,3],[4,5,6],[7,8]]

>>>reverse [1,2,3] : reverse [4,5,6] : [7,8] : []
[[3,2,1],[6,5,4],[7,8]]

(++) :: [a] -> [a] -> [a]
>>> [1] ++ []
[1]
>>> [1] ++ [2,3]
[1,2,3]

>>> [1,2,10] ++ [3] ++ [4]
[1,2,10,3,4]

>>>["aaaa"] ++ ["bbbb"]
["aaaa","bbbb"]
>>>"aaaa" ++ "bbbb"
"aaaabbbb"

>>>['a','a'] ++ "bbbb"
"aabbbb"

>>>[1,2,3] ++ [4,5,6] ++ [7,8] ++ []
[1,2,3,4,5,6,7,8]

>>>reverse [1,2,3] ++ reverse [4,5,6] ++ [7,8] ++ []
[3,2,1,6,5,4,7,8]

concat :: [[a]] -> [a]

>>>concat [[1,2,3],[4,5,6],[7,8],[]]
[1,2,3,4,5,6,7,8]

>>>concat [reverse [1,2,3], reverse [4,5,6], [7,8]]
[3,2,1,6,5,4,7,8]
-}



{-
>>>split 3 [1,2,3,4,5,6]
[1,2,3,6,5,4]
>>>split 4 [1,2,3,4,5,6,7,8]
[1,2,3,4,8,7,6,5]
-}
split :: Int -> [a] -> [a]
split n l = take n l ++ reverse(drop n l)

{-
>>>descombinaMetades' [(1,4),(2,5),(3,6)] 
[1,2,3,4,5,6]
>>>descombinaMetades' [(1,5),(2,6),(3,7),(4,8)]
[1,2,3,4,5,6,7,8]
-}

descombinaMetades :: [(a,a)] -> [a]
descombinaMetades l = split (length lista `div` 2) lista
    where lista = descombinaMetades' l
          descombinaMetades' :: [(a,a)] -> [a]
          descombinaMetades' [] = []
          descombinaMetades' l = [fst (head l)] ++ descombinaMetades' (tail l) ++ [snd (head l)]




descombinaMetades'' :: [(a,a)] -> [a]
descombinaMetades'' l = primeiros l ++ segundos l

{-
>>>primeiros [(10,5),(2,6),(3,7),(4,8)]
[10,2,3,4]

>>>segundos [(10,5),(2,6),(3,7),(4,8)]
[5,6,7,8]
-}

primeiros lst = if null lst then [] else fst (head lst) : primeiros (tail lst)
segundos lst = if null lst then [] else snd (head lst) : segundos (tail lst)




dm :: [(a,a)] -> [a]
dm l = dm' l [] []


{-
>>> dm' [] [] [] 
Ambiguous type variable ‘a0’ arising from a use of ‘evalPrint’
prevents the constraint ‘(Show a0)’ from being solved.
Probable fix: use a type annotation to specify what ‘a0’ should be.
These potential instances exist:
  instance Show OpenModule -- Defined in ‘Distribution.Backpack’
  instance Show OpenUnitId -- Defined in ‘Distribution.Backpack’
  instance Show FullUnitId
    -- Defined in ‘Distribution.Backpack.FullUnitId’
  ...plus 569 others
  (use -fprint-potential-instances to see them all)

>>> dm' [(1,5)] [] [] 
[1,5]

>>>dm' [(1,5),(2,6)] [] []
[1,2,5,6]
-}

dm' l ps ss = if null l then ps ++ ss
                        else dm' cauda (ps ++ [prim]) (ss ++ [segu])
    where cabeca = head l
          cauda = tail l
          prim = fst cabeca
          segu = snd cabeca




compacte' :: [[a]] -> [(a, Int)]
compacte' l
    | null l = []
    | otherwise = tupla : sublista
        where tupla = (char, num)
              char = head (head l)
              num = length (head l)
              sublista = compacte' (tail l)



{-
>>>partitions [1,2,3]
[[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]

-}
partitions [] = [[]]
partitions (x:xs) = [ x:e | e <- partitions xs] ++ partitions xs



{-
>>>partitions' 1
No instance for (Show (Int -> [[Int]]))
  arising from a use of ‘evalPrint’
  (maybe you haven't applied a function to enough arguments?)


-}
partitions'' :: Int -> [[Int]]
partitions'' n = partitions' n n

partitions' :: Int -> Int -> [[Int]]
partitions' _ 0  = [[]]
partitions' x y = [ h:t | h <- [1..x], t <- partitions' x (y-1)]



{-
>>>delta (1,2,3)
-8.0

>>>4/2*2
4.0

>>>4/(2*2)
1.0

>>>raízes (1,12,-13)
[1.0,-13.0]
-}

delta :: (Float, Float, Float) -> Float
delta (a,b,c) = b^2 - 4*a*c

raízes :: (Float,Float, Float) -> [Float]
raízes (a,b,c)
  | d > 0 = [raiz1,raiz2]
  | d == 0 = [raiz1]
  | otherwise  = []
  where d = delta (a,b,c)
        raiz1 =  (negate b + sqrt d)/(2*a)
        raiz2 =  (negate b - sqrt d)/(2*a)


{-
>>>maisMais [1,2,3] [4,5]
[1,2,3,4,5]

>>>maisMais [1,2] [4,5,6]
[1,2,4,5,6]

>>>maisMais [] [4,5]
[4,5]

>>>maisMais [1,2,3] []
[1,2,3]
-}

maisMais :: [a] -> [a] -> [a]
maisMais [] [] = []
maisMais [] (y:ys) = y : maisMais [] ys
maisMais (x:xs) y = x : maisMais xs y




{-
Escreva uma função **recursiva**  que calculo a soma dos quadrados dos números inteiros entre os parâmetros passados, inclusive.

Entrada:
    - i - Inteiro
    - n - Inteiro
Resultado
    - i^2 + (i+1)^2 + ... + n^2

>>>somaDosQuadrados 1 3
14

>>>somaDosQuadrados 3 6
86

>>>somaDosQuadrados 5 2
54

>>>somaDosQuadrados 3 (-2)
19

-}

somaDosQuadrados :: Int -> Int -> Int
somaDosQuadrados i n
    | i == n = i^2
    | i > n = somaDosQuadrados n i
    | otherwise = i*i + somaDosQuadrados (i+1) n



{-
Dado um período, escreva uma função que decida se é uma afirmação, interrogação, exclamação, ou nenhum.

Entrada:
    - periodo - String

Resultado:
    - "afirmacao", se período termina com ., ou "exclamacao" se período termina com !, ou "interrogacao" se período
    termina  com ?, ou "nada", caso contrário.
    Ignorar espaços no fim do período.

>>>tipoPeríodo "Oi."
"afirmacao"

>>>tipoPeríodo "Oi?"
"interrogacao"

>>>tipoPeríodo "Oi!"
"exclamacao"

>>>tipoPeríodo "Oi"
"nada"

>>>tipoPeríodo "Oi^"
"nada"

>>>tipoPeríodo "Oi."
"afirmacao"

>>>tipoPeríodo "Oi?     "
"interrogacao"

-}

tipoPeríodo :: String -> String
tipoPeríodo período = case last períodoTrimmed of '.' -> "afirmacao"
                                                  '?' -> "interrogacao"
                                                  '!' -> "exclamacao"
                                                  _   -> "nada"
                      where períodoTrimmed = [c | c <- período, c /= ' ']



{-
Defina uma função que remova as primeiras duplicatas de uma lista de inteiros.

Entrada:
    - l - lista de inteiros.

Resultado:
    - lista em que as primeiras ocorrências repetidas de qualquer valor foram removidas.

>>>removeDuplicatas [1,2,3,4,5,3,7,8,3]
[1,2,4,5,7,8,3]

-}

removeDuplicatas :: [Int] -> [Int]
removeDuplicatas [] = []
removeDuplicatas (x:xs) | x `elem` xs = removeDuplicatas xs
                        | otherwise   = x:removeDuplicatas xs



{-
Defina uma função que remova as últimas duplicatas de uma lista de inteiros.

Entrada:
    - l - lista de inteiros.

Resultado:
    - lista em que as primeiras ocorrências repetidas de qualquer valor foram removidas.

>>>removeDuplicatas2 [1,2,3,4,5,3,7,8,3]
[1,2,3,4,5,7,8]

-}

removeDuplicatas2 :: [Int] -> [Int]
removeDuplicatas2 l = reverse (removeDuplicatas (reverse l))



{-
Mastermind 1.

Dado uma lista com 4 de 8 possíveis cores, determinar se a lista é válida.

Entrada:
    - l - Lista de inteiro, onde cada inteiro vai de 1 a 8 e representa uma cor.

Resultado:
    - True se não há repetições e tem tamanho 4
    - False se há repetições ou tamanho diferente de 4

>>>mmVálido [1,2,3,4]
True

>>>mmVálido [1,2,3,9]
False

>>>mmVálido [1,2,3,4,5]
False

>>>mmVálido [1,2,4,4]
False

-}

mmVálido :: [Int] -> Bool
mmVálido l = length l == 4 && removeDuplicatas l == l && coresVálidas l

coresVálidas l = all (>0) l && all (<9) l

{-
Mastermind 2

Dado duas listas, se alguma não é válida, lançar uma exceção (use error).
Se as duas são válidas, retornar uma tupla com a quantidade de acertos bons e ótimos da jogada.

Entrada
    - config - lista de inteiro
    - jogada - lista de inteiro

Resultado
    - tupla (o,b) onde o é um inteiro com a quantidade de cores em jogada e que aparecem na mesma posição em config
    e b é quantidade de cores em jogada e que aparecem em posições diferentes em config.


>>>tentativa [1,2,3,4] [5,6,7,8]
(0,0)

>>>tentativa [1,2,3,4] [1,2,3,4]
(4,0)

>>>tentativa [1,2,3,4] [4,3,2,1]
(0,4)

>>>tentativa [1,2,3,4] [2,1,3,4]
(2,2)

>>>tentativa [1,2,3,4,5] [2,1,3,4]
não válido
-}

tentativa :: [Int] -> [Int] -> (Int,Int)
tentativa config jogada
    | mmVálido config && mmVálido jogada = (otimos, bons - otimos)
    | otherwise = error "não válido"
    where otimos = contaÓtimo config jogada
          bons = contaBom config jogada

contaÓtimo :: [Int] -> [Int] -> Int
contaÓtimo [] _ = 0
contaÓtimo _ [] = 0
contaÓtimo (x:xs) (y:ys) = (if x == y then 1 else 0) + contaÓtimo xs ys

contaBom :: [Int] -> [Int] -> Int
contaBom [] _ = 0
contaBom _ [] = 0
contaBom (x:xs) y = (if x `elem` y then 1 else 0) + contaBom xs y




corDoNaipe :: Naipe -> String
corDoNaipe Copas = "Vermelho"
corDoNaipe Ouro = "Vermelho"
corDoNaipe Paus = "Preto"
corDoNaipe Espada = "Preto"

corDoNaipe' :: Naipe -> String
corDoNaipe' n = case n of
        Copas -> "Vermelho"
        Ouro -> "Vermelho"
        Paus -> "Preto"
        Espada -> "Preto"

{-
>>>corDoNaipe Copas
"Vermelho"

>>>corDoNaipe' Copas
"Vermelho"

-}


{-
>>>maisMais' [1,2,3] [4,5,6]
[1,2,3,4,5,6]
-}

maisMais' :: [a] -> [a] -> [a]
maisMais' [] [] = []
maisMais' [] y = y
maisMais' (x:xs) y = x : maisMais' xs y



take' :: Int -> [a] -> [a]
take' _ [] = []
take' n (x:xs) = if n <= 0
                 then []
                 else x : take' (n-1) xs



drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' n l@(_:xs)
    | n <= 0 = l
    | otherwise = drop' (n-1) xs


{-
>>>união [1..4] [2..6]
>>>união [1..4] [5..6]
>>>união [1,1,2] [2,3,5]
[1,2,3,4,5,6]
[1,2,3,4,5,6]
[1,2,3,5]
-}

união :: [Int] -> [Int] -> [Int]
união [] [] = []
união [] (y:ys) = if y `elem` ys
                  then união [] ys
                  else y : união [] ys
união (x:xs) y = if x `elem` y || x `elem` xs
                 then união xs y
                 else x : união xs y


{-
>>>união' [1..4] [2..6]
>>>união' [1..4] [5..6]
>>>união' [1,1,2,4] [2,3,5]
[1,2,3,4,5,6]
[1,2,3,4,5,6]
[1,4,2,3,5]

-}

união' :: [Int] -> [Int] -> [Int]
união' l1 l2 = [e | e <- l11, e `notElem` l22] ++ l22
    where l11 = unique l1
          l22 = unique l2


unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x:xs)
    | x `elem` xs = unique xs
    | otherwise = x : unique xs



data Valor = Ás | Número Int | Valete | Dama | Rei | Letra Char
    deriving (Eq,Show, Ord)

{-
>>>prop_valor (Número 3)
True

>>>prop_valor (Número 23)
False

>>>prop_valor Rei
True

-}

prop_valor (Número n) =  n > 1 && n < 11
prop_valor _ = True


{-
>>> remove 1 [2,3,1,4,1,5,1]
[2,3,4,1,5,1]

>>> bubble [2,3,1,4,1,5,1]
[1,1,1,2,3,4,5]
-}



remove :: Int -> [Int] -> [Int]
remove e [] = []
remove e (x:xs) | e == x = xs
                | otherwise = x:remove e xs

selection :: [Int] -> [Int]
selection [] = []
selection l = menor : selection restante
    where menor = minimum l
          restante = remove menor l






{-
>>>fr 1 [1,2,3]
[2,3]

>>>fr 1 [5,1,2,3]
[5,2,3]

>>>fr 1 [2,3,1]
[2,3]

>>>fr 1 []
[]

>>>fr 1 [2,3]
[2,3]

-}

fr :: Int -> [Int] -> [Int]
fr i []     = []
fr i (x:xs) = if i == x then xs
                        else x:fr i xs

{-
>>>fm [1,2,3]
1

>>>fm [2,1,3]
1



>>>fm [2,3,1]
fm [2,3,1] = min 2 (fm [3,1])
           = min 2 (min 3 (fm [1]))
           = min 2 (min 3 (1))
           = min 2 (1)
           = 1
1

>>>fm []
Lista vazia
-}

fm :: [Int] -> Int
fm []     = error "Lista vazia"
fm [x]    = x
fm (x:xs) = min x (fm xs)


{-
>>>fs [3,1,2]
[1,2,3]

-}


fs :: [Int] -> [Int]
fs [] = []
fs l =
    let m = fm l
        r = fr m l
    in m:fs r



fs' :: [Int] -> [Int]
fs' [] = []
fs' l  = let m = minimum l
             r = delete m l
         in m:fs r

{-
>>>fd []
([],[])

>>>fd [1]
([],[1])

>>>fd [1..5]
([1,2],[3,4,5])

>>>fd [1..6]
([1,2,3],[4,5,6])

-}
fd :: [Int] -> ([Int],[Int])
fd l = fd' (length l `div` 2) l
    where   fd' _ []     = ([],[])
            fd' 0 l        = ([],l)
            fd' tle (x:xs) = (x:le, ld)
                where (le,ld) = fd' (tle -1) xs



-- >>>f [1,3..30]
-- ([1,3,5,7,9,11,13],[15,17,19,21,23,25,27,29])
f l = (take (length l `div` 2) l, drop (length l `div` 2) l)

{-
>>>fu [1..10] [1..10]
[1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10]

-}

fu :: [Int] -> [Int] -> [Int]
fu x [] = x
fu [] x = x
fu lx@(x:xs) ly@(y:ys)
  | x <= y    = x : fu xs ly
  | otherwise = y : fu lx ys

{-
>>>fms [10,9..(-10)]
[-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10]
-}

fms :: [Int] -> [Int]
fms [] = []
fms [e] = [e]
fms l = let (le,ld) = fd l
        in fu (fms le) (fms ld)



data Pressão = Psi Float | Bar Float

converteParaBar :: Pressão -> Pressão
converteParaBar v@(Bar _)  = v
converteParaBar v@(Psi vp) = Bar (vp * 1.6)

-- >>> vempsi = Psi 30

-- >>> vembar = Bar 45

-- >>> qsort [10,9..1]
-- [1,2,3,4,5,6,7,8,9,10]

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort (filter (<= x) xs) ++ [x] ++ qsort (filter (> x) xs)

sortAndPrint :: (Show a, Ord a) => [a] -> [String]
sortAndPrint l = map show (qsort l)

-- >>> sortAndPrint [10,9..1]
-- ["1","2","3","4","5","6","7","8","9","10"]



type Item = String

éJogador i = isPrefixOf "Jogador_" i

idDoJogador j = ord (last j) - ord '0'

-- >>> idDoJogador "Jogador_3" 
-- 3

jogadores = [(1,(10,2)),(3,(10,2)),(2,(10,2))]


-- >>>fst' (1,2)
-- 1
fst' :: (a,b) -> a
fst' (v1,_) = v1







data Ponto = PR1 Int | PR2 Int Int | PR3 Int Int Int

qualR :: Ponto -> String
qualR (PR1 x)     = "R1 x=" ++ show x
qualR (PR2 x y)   = "R2 x=" ++ show x ++ " y=" ++ show y
qualR (PR3 x y z) = "R3"

-- >>> qualR (PR2 10 10)
-- "R2 x=10 y=10"




data Mão = Pedra | Tesoura | Papel deriving (Eq,Show)

data Jogo = Empate | Ganha Mão deriving (Show,Eq)

quemGanha :: Mão -> Mão -> Jogo
quemGanha Pedra Tesoura = Ganha Pedra
quemGanha Pedra Papel   = Ganha Papel
quemGanha Tesoura Pedra = Ganha Pedra
quemGanha Tesoura Papel = Ganha Tesoura
quemGanha Papel Pedra   = Ganha Papel
quemGanha Papel Tesoura = Ganha Tesoura
quemGanha _ _           = Empate

jogo' :: Mão -> Mão -> Jogo
jogo' Pedra Tesoura = Ganha Pedra
jogo' Tesoura Papel = Ganha Tesoura
jogo' Papel Pedra   = Ganha Papel
jogo' mao1 mao2 = if mao1 == mao2
                     then Empate
                     else Ganha mao2

-- >>>quemGanham [(Pedra,Pedra),(Pedra,Tesoura),(Pedra,Papel)]
-- [Empate,Ganha Pedra,Ganha Papel]

quemGanham :: [(Mão,Mão)] -> [Jogo]
quemGanham []               = []
quemGanham ((m1,m2):tms)    = quemGanha m1 m2 : quemGanham tms



-- >>>quemGanham' [(Pedra,Pedra),(Pedra,Tesoura),(Pedra,Papel)]
-- [Empate,Ganha Pedra,Ganha Papel]

-- >>>quemGanham' []
-- []

quemGanham' :: [(Mão,Mão)] -> [Jogo]
quemGanham' l = [ quemGanha m1 m2 | (m1,m2) <- l]



presenca :: String -> Bool
presenca a = True

-- >>> listaPresenca ["huryel", "marcus", "jose"]
-- ["marcus estah presente"]
listaPresenca l = [ e ++ " estah presente" | e <- l, head e == 'm']


l = [1..]



data Operação = Add Int Int | Mult Int Int | Neg Int

executaOperacao :: Operação -> Int
executaOperacao (Add i1 i2)  = i1 + i2
executaOperacao (Mult i1 i2) = i1 * i2
executaOperacao (Neg i1)     = negate i1

data Exp = EAdd Exp Exp | ENeg Exp | ENum Int

executaExpressao :: Exp -> Int
executaExpressao (ENum i) = i
executaExpressao (ENeg e) = negate (executaExpressao e)
executaExpressao (EAdd e1 e2) = executaExpressao e1 + executaExpressao e2

-- >>>executaExpressao (ENum 3)
-- 3
-- >>>executaExpressao (ENeg (ENum 3)) = negate (executaExpressao (ENum 3)) = 

-- >>>executaExpressão (EAdd (ENum 3) (ENum 4)) = 3 + 4 = 7
--                    executaExpressao (ENum 3) = 3
--                    executaExpressao (ENum 4) = 4

-- >>>executaExpressão (EAdd (ENum 3) (ENeg (ENum 4))) = 3 + (-4) = -1
-- executaExpressao (ENum 3) = 3
-- executaExpressao (ENeg (ENum 4)) = negate (4) = -4
-- executaExpressao (ENum 4) = 4



multiplicarPor10 :: Int -> Int
multiplicarPor10 x = x * 10

multiplicarPor10Lista :: [Int] -> [Int]
multiplicarPor10Lista l = [multiplicarPor10 e | e <- l]

soma3 :: Int -> Int
soma3 x = x + 3

soma3Lista :: [Int] -> [Int]
soma3Lista l = [soma3 e | e <- l]

éPar :: Int -> Bool
éPar x = even x

éParLista :: [Int] -> [Bool]
éParLista l = [éPar e | e <- l]

mod3 :: Int -> Int
mod3 x = x `mod` 3

mod3List :: [Int] -> [Int]
mod3List l  = [mod3 e | e <- l]


mapeie :: (a -> b) -> [a] -> [b]
mapeie f xs = [f x | x <- xs]

-- >>> mapeie multiplicarPor10 [1..10]
-- [10,20,30,40,50,60,70,80,90,100]

-- >>> mapeie soma3 [1..10]
-- [4,5,6,7,8,9,10,11,12,13]

-- >>> mapeie éPar [1..10]
-- [False,True,False,True,False,True,False,True,False,True]

-- >>> mapeie mod3 [1..10]
-- [1,2,0,1,2,0,1,2,0,1]




filtra [] = []
filtra (Empate:xs) = filtra xs
filtra (Ganha m:xs) = m: filtra xs

-- >>> filtra [Empate, Ganha Pedra, Empate, Ganha Tesoura]
-- [Pedra,Tesoura]

filtra' l = [ extraiMao e |e <- l, e /= Empate]
    where extraiMao (Ganha m) = m
          extraiMao Empate = error "Deu prego"

-- >>> filtra' [Empate, Ganha Pedra, Empate, Ganha Tesoura]
-- [Pedra,Tesoura]

filtra'' l = [ m | Ganha m <- l]

-- >>> filtra'' [Empate, Ganha Pedra, Empate, Ganha Tesoura]
-- [Pedra,Tesoura]


somar x y = x + y

aplicarLista1 :: (a -> a -> a) -> a -> [a] -> a
aplicarLista1 f i [] = i
aplicarLista1 f i (n:ns) = n `f` (aplicarLista1 f i ns)

-- >>> aplicarLista1 somar 0 [1..5] 
-- 15

-- >>> aplicarLista1 (++) "" ["lala","lele","lili"] 
-- "lalalelelili"


-- >>> aplicarLista1 (||) False [False,False,False, False]
-- False




somaQuads :: Int -> Int
somaQuads n = foldr (+) 0 [e^2 | e <- [1..n]]


-- n ===> [1..n]

quadMais n1 n2 = n1*n1 + n2

-- foldr o i [1..n] ===> 1 `o` (2 `o` (3 `o` i))
--                  ===> 1^2 + (2^2 + (3^2 + 0)

-- >>>foldr o 0 [1..3]
-- 14

somaQuads' n = foldr quadMais 0 [1..n]

-- >>> somaQuads' 3
-- 14


somaMais' n1 n2 = n1 + n2*n2

-- foldl o i [1..3] ===> ((i `o` 1) `o` 2) `o` 3
--                  ===> ((i + 1^2) + 2^2) + 3^2

-- >>> foldl somaMais' 0 [1..3]
-- 14
{-
comp [2..4] ===> 3

foldl o i [2,3,4] ===> ((i `o` 2) `o` 3) `o` 4
                       ((0  +  1)  +  1)  +  1

>>>foldl opC' 0 [2,3,4]
3
-}

opC' a _ = a + 1



op n1 n2 = n1 `div` n1 + n2

opC _ b = 1 + b

comp :: Foldable t => t Int -> Int
comp l = foldr op 0 l




-- >>> foldl1 min [10100,10099..1010]
-- 1010


-- >>>maxBound::Int
-- 9223372036854775807


concatInvertido a b = b ++ [a]

{-

reverte [1,2,3] = foldr o i [1,2,3] ===> 1 `o` (2 `o` (3 `o` i))

                                                        [3] ++ []
                                                 [3] ++ [2]
                                         [3,2] ++ 1       
                                        [3,2,1]
                                         

>>> foldr concatInvertido [] [1,2,3]
[3,2,1]




Usando foldr, defina uma função que recebe duas listas e que remova todo elemento da segunda lista que aparece na primeira lista.
>>>removeDups [1,2,4] [0,1,2,3]
[0,3]

foldr op [] [0,1,2,3] ===> 0 `op` (1 `op` (2 `op` (3 `op` i)))
                                                          []
                                                     [3]
                                             [3]
                                     [3]
                            [0,3]
-}


removeDups l1 l2 = foldr op [] l2
    where op e l = if e `elem` l1 then l else e:l






o :: (b -> c) -> (a -> b) -> (a -> c )
o f g x = f (g x)

{-
>>>f x = x*x
>>>g x = x+10

>>>fog = f `o` g

>>> fog 3
169
-}





-- 5 - Usando foldl, defina uma função que recebe duas listas e que remova todo elemento da segunda lista que aparece na primeira lista.

remove' l1 l2 = foldl op [] l2
   where op l e = if e `elem` l1 then l else l++[e]

-- >>> remove' [-1,1,4] [0,1,2,3,4]
-- [0,2,3]

-- foldl o i l2 

-- foldl o i [0,1,2,3,4] ===>  (((((i `o` 0) `o` 1) `o` 2) `o` 3) `o` 4)
--                                  []++[0]
--                                    [0]
--                                           [0]
--                                               [0]++[2]
--                                                 [0,2]
--                                                      [0,2]++[3]
--                                                        [0,2,3]
--                                                               [0,2,3]
-- >>> foldl op [] [0,1,2,3,4]

