

{-


somaDeAte 2 4 = 2 + somaDeAte (2+1) 4 = 2 + 7 = 9
                    somaDeAte 3 4 = 3 + somaDeAte (3+1) 4 = 3 + 4 = 7
                                        somaDeAte 4 4 = 4


>>> somaDeAte 3 (4 + 7)
Variable not in scope: somaDeAte :: t0 -> t1 -> t


x + (y == true)? 3 : 4 ;

>>> maiorDeTres 1 42 2
42

>>> éZero 0
True

>>> éZero (-1)
False

>>> 
-}



maiorDeTres a b c
  | a >= b && a >= c   = a
  | b >= c             = b
  | otherwise          = c

éZero x
  | x < 0 = False 
  | x == 0 = True
