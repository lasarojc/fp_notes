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