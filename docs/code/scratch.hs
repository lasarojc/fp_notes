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
    |otherwise = error "Indefinido"



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
