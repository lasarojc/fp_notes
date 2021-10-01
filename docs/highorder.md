# Funções de alta ordem


Iteração

```hs
mapeie :: (a -> b) -> [a] -> [b]
mapeie f xs = [f x | x <- xs]

filtre :: (a -> Bool) -> [a] -> [a]
filtre p xs = [x | x <- p x]

descarte :: (a -> Bool) -> [a] -> [a]
descarte p xs = [x | x <- not (p x)]
```

Sumarização

```hs
soma []     = 0
soma (x:xs) = x + soma xs

eLógico []     = True
eLógico [x:xs] = x && eLógico xs


dobreDireita op base []     = base 
dobreDireita op base (x:xs) = x `op` dobreDireita op base xs

dobreDireita (+) 0 [1,2,3,4,5]
dobreDireita (*) 1 [1,2,3,4,5]
```

```hs
dobreEsquerda?
```


sortOn :: Ord b => (a -> b) -> [a] -> [a]#

Sort a list by comparing the results of a key function applied to each element. sortOn f is equivalent to sortBy (comparing f), but has the performance advantage of only evaluating f once for each element in the input list. This is called the decorate-sort-undecorate paradigm, or Schwartzian transform.



## Lambda



all
https://hoogle.haskell.org/?q=all