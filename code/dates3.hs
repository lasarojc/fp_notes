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
dataEhMenor :: Data -> Data -> Bool
dataEhMenor (de,me,ae) (dd,md,ad) = (ae < ad) || (ae == ad && me < md) || (ae == ad && me == md && de < dd)

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
dataEhMenor' :: Data -> Data -> Bool
dataEhMenor' (de,me,ae) (dd,md,ad) = (ae,me,de) < (ad,md,dd)
