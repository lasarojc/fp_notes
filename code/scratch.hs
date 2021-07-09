{- 
>>>fat 0
1

>>>fat 5
120

>>>fat (-2)
Indefinido

>>>show [1..(-3)]
"[]"
-}

fat 0 = 1
fat n 
    |n > 0 = n * fat (n-1)
    |otherwise = error "Indefinido"
