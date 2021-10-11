somar :: Int -> Int -> Int 
somar x y = x + y

multiplicar :: Int -> Int -> Int 
multiplicar x y = x * y

dividir :: Int -> Int -> Int 
dividir x y = x `div` y


operar :: (Int -> Int -> Int) -> Int -> Int -> Int
operar f x y = f x y

-- >>> operar somar 3 4
-- 7

-- >>> operar multiplicar 3 4
-- 12

-- >>> operar dividir 3 4
-- 0