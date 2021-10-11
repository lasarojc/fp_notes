multiplicarPor10 :: Int -> Int
multiplicarPor10 x = x * 10

multiplicarPor10Lista :: [Int] -> [Int]
multiplicarPor10Lista l = [multiplicarPor10 e | e <- l]

-- >>>multiplicarPor10Lista [2..5]
-- [20,30,40,50]

somar3 :: Int -> Int
somar3 x = x + 3

somar3Lista :: [Int] -> [Int]
somar3Lista l = [somar3 e | e <- l]
-- >>>somar3Lista [2..5]
-- [5,6,7,8]


éPar :: Int -> Bool
éPar x = even x

éParLista :: [Int] -> [Bool]
éParLista l = [éPar e | e <- l]
-- >>>éParLista [2..5]
-- [True,False,True,False]


mod3 :: Int -> Int
mod3 x = x `mod` 3 

mod3Lista :: [Int] -> [Int]
mod3Lista l  = [mod3 e | e <- l]

-- >>>mod3Lista [2..5]
-- [2,0,1,2]
