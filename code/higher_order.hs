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


aplicarLista1 :: (Int -> Int -> Int) -> Int -> [Int] -> Int
aplicarLista1 f i [] = i
aplicarLista1 f i (n:ns) = f n (aplicarLista1 f i ns)

-- >>>aplicarLista1 somar 0 [1..5]
-- 15

-- >>>aplicarLista1 multiplicar 1 [1..5]
-- 120

aplicarLista2 :: (Int -> Int -> Int) -> Int -> [Int] -> Int
aplicarLista2 f i [] = i
aplicarLista2 f i (n:ns) = aplicarLista2 f (f i n) ns

-- >>>aplicarLista2 somar 0 [1..5]
-- 15

-- >>>aplicarLista2 multiplicar 1 [1..5]
-- 120