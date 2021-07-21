{- 
>>>fibWhere 10
55
-}
fibWhere 0 = 0
fibWhere 1 = 1
fibWhere n = prev + prevPrev
    where prev     = fibPattern (n - 1) 
          prevPrev = fibPattern (n - 2)

{-
>>>fibUp 1000
70330367711422815821835254877183549770181269836358732742604905087154537118196933579742249494562611733487750449241765991088186363265450223647106012053374121273867339111198139373125598767690091902245245323403501
-}
fibUp 0 = 0
fibUp 1 = 1
fibUp n = fibUpTo 0 1 1 n
    where fibUpTo prevPrev prev prevCount limit = if prevCount == limit then prev + prevPrev
                                                                        else fibUpTo prev (prev + prevPrev) (prevCount + 1) limit
