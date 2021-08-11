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


data Naipe = Copas | Espada | Ouro | Paus deriving (Ord,Eq)




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