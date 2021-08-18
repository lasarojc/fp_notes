# Mais listas

## Casamento de padrões
Toda lista é ou uma lista vazia, ou um elemento como cabeça e uma lista como cauda.
Assim, se cobrir estes dois casos em uma definição por casamento de padrões, terá coberto todos os casos.

```hs
oQueHáNaCabeça :: (Show a) => [a] -> String
oQueHáNaCabeça [] = "Nada"
oQueHáNaCabeça (x:xs) = "Há " ++ x
```

Isso não quer dizer que não possa ser mais específico no padrão e usar o casamento de padrões para extrair mais elementos do início da lista.

```hs
oQueHáNaLista :: (Show a) => [a] -> String
oQueHáNaLista [] = "Nada"
oQueHáNaLista [x] = "Só " ++ x
oQueHáNaLista [x1,x2] = "Há " ++ x1 ++ " e " ++ x2
oQueHáNaLista (x:xs) = "Há " ++ x ++ " e mais um monte de coisas" 
```


```hs
resumo :: [a] -> "String"
resumo [] -> "Nada"
resumo [_] -> "Um"
resumo [_,_] -> "Dois"
resumo _ -> "Muitos"
```

## Recursão

```hs
resumo :: [String] -> "String"
resumo [] -> "Nada"
resumo [e] -> "Só " ++ e
resumo [e1,e2] -> e1 ++ " e " ++ e2
resumo (e1:resto) -> e1 ++ 
                    " um monte de coisas, terminando com " ++ 
                    last rest
```


$maximum~[1,2,3] = max~1 \left( maximum~[2,3] = max~2 \left( maximum~[3] = 3 \right)        \right)$


```hs
maximum [] = error "lista vazia"  
maximum [h] = h
maximum (h:t) = max h (maximum t)
```

comprimento

```hs
comprimento :: [a] -> Int
comprimento [] = 0
comprimento (x:xs) = 1 + len xs
```

último

```hs
último :: [a] -> a
último [] = error "List is empty"
último [x] = x
último (_:xs) = last xs
``` 

inverso

```hs
inverso :: [a] -> [a]
inverso [] = []
inverso (x:xs) = inverso xs ++ [x]
```

## Compreensão de Listas

a função cycle
