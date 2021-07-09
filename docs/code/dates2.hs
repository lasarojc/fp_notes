type Data = (Int, Int, Int)


{-
>>> dataEhMenor (01,01,2000) (02,01,2000)
True

>>> dataEhMenor (05,01,2000) (02,01,2000)
False

>>> dataEhMenor (01,01,2000) (02,01,2000)
True

>>> dataEhMenor (05,01,2000) (02,02,2000)
True

>>> dataEhMenor (05,05,2000) (02,02,2001)
True
-}



dataEhMenor (de,me,ae) (dd,md,ad) = (ae < ad) || (ae == ad && me < md) || (ae == ad && me == md && de < dd)
{- 
>>> bissexto 3
False

>>> bissexto 20
True

>>> bissexto 100
False

>>> bissexto 2000
True
-}

bissexto :: Integral a => a -> Bool
bissexto x
  | mod x 400 == 0 = True
  | mod x 100 == 0 = False
  | otherwise = mod x 4 == 0

{- 
>>> dataValida (1,1,2001)
True

>>> dataValida (1,13,2001)
False

>>> dataValida (29,2,2000)
True

>>> dataValida (29,2,1999)

-}

dataValida :: (Int, Int, Int) -> Bool
dataValida (d,m,a) = 
    anoValido a && mesValido m && diaValido d m (bissexto a)

{-
>>> diaValido 2 1 False
True

>>> diaValido 2 1 True
True

>>> diaValido 29 2 False
False

>>> diaValido 29 2 True
True

-}

diaValido :: Int -> Int -> Bool -> Bool
diaValido d m bi = d > 0 && d <= numDias m bi

{-
>>> numDias 3 False
31

>>> numDias 2 True
29

>>> numDias 2 False
28

-}
numDias :: Int -> Bool -> Int
numDias m bi
  | m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12 = 31
  | m == 2 = if bi then 29 else 28
  | otherwise = 30

{- 
>>> mesValido 4
True

>>> mesValido 13
False
-}

mesValido :: Int -> Bool
mesValido m = m > 0 && m < 13

{- 
>>> anoValido 3000
True
-}
anoValido :: Int -> Bool
anoValido a = a > 0
