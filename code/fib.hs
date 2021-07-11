{-
>>> fibIf 4
3
-}
fibIf n = if n == 1 then 0
          else if n == 2 then 1
          else fibIf (n - 1) + fibIf (n - 2)

fibGuard n
  | n == 1 = 0
  | n == 2 = 1
  | otherwise = fibGuard (n - 1) + fibGuard (n - 2)

fibPattern 1 = 0
fibPattern 2 = 1
fibPattern n = fibPattern (n - 1) + fibPattern (n - 2)
