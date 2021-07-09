{- 
>>>bissexto 5
False

>>>bissexto 200
False

>>>bissexto 2000
True

>>>bissexto 1996
True

>>>bissexto 1900
False

-}

bissexto x
  | mod x 4 /= 0 = False       -- Não divisíveis por 4
  | mod x 400 == 0 = True      -- Divisíveis por 4 e por 400
  | mod x 100 == 0 = False     -- Divisíveis por 4, não por 400, e por 100
  | otherwise = True           -- Divisíveis por 4, e não por 100
