collatz :: Int -> Int -> Int
collatz n i
    | i == 1 = n
    | even (collatz n (i-1)) = collatz n (i-1) `div` 2
    | otherwise = collatz n (i-1) * 3 + 1

converge :: Int -> Int -> Int -> Bool
converge n passo limite
    | limite == 0 = error "Não alcançou uma resposta"
    | collatz n passo == 1 = True
    | otherwise = converge n (passo + 1) (limite - 1)
