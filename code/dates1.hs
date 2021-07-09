type Data = (Int, Int, Int)

dataEhMenor :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
dataEhMenor (de,me,ae) (dd,md,ad) = (ae < ad) || (ae == ad && me < md) || (ae == ad && me == md && de < dd)
