{-
>>> fibIf 4
3
-}
fibIf n = if n == 0 then 0
          else if n == 1 then 1
          else fibIf (n - 1) + fibIf (n - 2)

fibGuard n
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = fibGuard (n - 1) + fibGuard (n - 2)

fibPattern 0 = 0
fibPattern 1 = 1
fibPattern n = fibPattern (n - 1) + fibPattern (n - 2)

fibCase x = case x of 0 -> 0
                      1 -> 1
                      n -> fibCase (n - 1) + fibCase (n - 2)
