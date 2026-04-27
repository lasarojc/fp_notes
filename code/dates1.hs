type Data = (Int, Int, Int)

dataEhMenor :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
dataEhMenor (de,me,ae) (dd,md,ad) = (ae < ad) || (ae == ad && me < md) || (ae == ad && me == md && de < dd)


dataEhMenor' :: Data -> Data -> Bool
dataEhMenor' (de,me,ae) (dd,md,ad) = if ae < ad then True
                                                else if me < md then True 
                                                                else if de < dd then True
                                                                                else False
