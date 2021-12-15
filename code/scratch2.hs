

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



-- >>> soma2v (1,4) (3,4)
-- (4,8)

soma2v :: (Int,Int) -> (Int,Int) -> (Int,Int)
soma2v        p1          p2    =
   let p1x = fst p1 
       p1y = snd p1
       p2x = fst p2
       p2y = snd p2
       prx = p1x + p2x
       pry = p1y + p2y
   in (prx, pry)


-- >>> soma2v' (1,4) (3,4)
-- (4,8)

soma2v' (x1,y1) (x2,y2) = (x1+x2, y1+y2)

-- >>> prim (1,2,3,4)
-- 2

prim :: (a, b, c, d) -> b
prim (_,x,_,_) =  x

prim' :: (a,b,c) -> b
prim' (_,x,_) =  x


-- >>> f 1
--(False, 1)

-- >>> f (-1)
--(True, 1)

