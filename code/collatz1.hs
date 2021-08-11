collatz :: Int -> Int -> Int
collatz n i
    | i == 1 = n
    | even (collatz n (i-1)) = collatz n (i-1) `div` 2
    | otherwise = collatz n (i-1) * 3 + 1


converge :: Int -> Bool
converge n = convergeInterna n 1


convergeInterna :: Int -> Int -> Bool
convergeInterna n passo
    | collatz n passo == 1 = True
    | otherwise = convergeInterna n (passo + 1)
