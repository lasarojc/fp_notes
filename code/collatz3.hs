collatz :: Int -> Int -> Int
collatz n i
    | i == 1 = n
    | even anterior = anterior `div` 2
    | otherwise = anterior * 3 + 1
    where anterior = collatz n (i-1)

converge :: Int -> Int -> Bool
converge n passo
    | termo == 1 = True
    | otherwise = converge n (passo + 1)
    where termo = collatz n passo