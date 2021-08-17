{- 
>>>fibWhere 10
55
-}
fibWhere 0 = 0
fibWhere 1 = 1
fibWhere n = prev + prevPrev
    where prev     = fibWhere (n - 1) 
          prevPrev = fibWhere (n - 2)