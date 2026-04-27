fibUp :: Integer  -> Integer 
fibUp 0 = 0
fibUp 1 = 1
fibUp n = fibUpTo 0 1 1 n
    where fibUpTo prevPrev prev prevCount limit = if prevCount == limit then prev + prevPrev
                                                                        else fibUpTo prev (prev + prevPrev) (prevCount + 1) limit
