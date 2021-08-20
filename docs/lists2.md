
## Casamento de padrões
Toda lista é ou uma lista vazia, ou um elemento como cabeça e uma lista como cauda.
Assim, se cobrir estes dois casos em uma definição por casamento de padrões, terá coberto "todos" os casos!
Mas como? Vejamos um exemplo.

```hs
oQueHáNaCabeça :: (Show a) => [a] -> String
oQueHáNaCabeça [] = "Nada"
oQueHáNaCabeça (x:xs) = "Há " ++ x
```

* Linha 1: não se preocupe esta linha; ela apenas implica que a lista deve ser de valores convertíveis a String
* Linha 2: esta linha usa um padrão constante para testar se a lista é vazia, isto é, `#!hs []` e, neste caso, retornar a string `#!hs "Nada"` como resultado.
* Linha 3: este o caso mais interessante, pois usa um padrão que define uma em que `#!hs x` é a cabeça, concatenada por `#!hs :` a uma calda `#!hs xs`, e retorna `#!hs "Há "` seguido do valor casado com `#!hs x`.

Observe que foram usados parênteses na linha 3 para especificar padrão, e não apenas `#!hs x:xs` como seria de se esperar.
A verdade é que o padrão é `#!hs x:xs` e os parêntesis são usados apenas para impedir que o Haskell primeiro avalie `#!hs oQueHáNaCabeça x` antes de avaliar o operador `#!hs :` e o seu segundo operando.
Esta é uma idiossincrasias do Haskell com a qual você simplesmente terá que aprender a conviver para dividir listas entre cabeça e cauda em um casamento de padrões.

Embora estes dois padrões, isto é, lista vazia e cabeça seguida de cauda, cubram todas as possibilidades de listas, não quer dizer que não haja melhores opções, dependendo do que precise extrair da lista.
Por exemplo, na próxima função há 4 casos de casamento de padrões distintos:

```hs
oQueHáNaLista :: (Show a) => [a] -> String
oQueHáNaLista [] = "Nada"
oQueHáNaLista [x] = "Só " ++ (show x)
oQueHáNaLista [x1,x2] = "Há " ++ (show x1) ++ " e " ++ (show x2)
oQueHáNaLista (x:xs) = "Há " ++ (show x) ++ " e mais um monte de coisas" 
```

* Linha 2: lista vazia;
* Linha 3: lista com exatamente um elemento, casado com `#!hs x`.
* Linha 4: lista com exatamente 2 elementos, casados com `#!hs x1` e `#!hs x2`.
* Linha 5: lista com mais de 2 elementos, em que a cabeça é casada com `#!hs x` e a cauda com `#!hs xs`.

Outra forma de obter exatamente o mesmo resultado, usando mais o operador `#!hs cons`,  seria a seguinte.

```hs
oQueHáNaLista :: (Show a) => [a] -> String
oQueHáNaLista [] = "Nada"
oQueHáNaLista (x:[]) = "Só " ++ (show x)
oQueHáNaLista (x1:x2:[]) = "Há " ++ (show x1) ++ " e " ++ (show x2)
oQueHáNaLista (x:xs) = "Há " ++ (show x) ++ " e mais um monte de coisas" 
```

Dado que strings são apenas listas de Char, estas também podem ser decompostas por casamentos de padrões.

```hs
resumo :: [a] -> "String"
resumo [] -> "Nada"
resumo [_] -> "Um"
resumo [_,_] -> "Dois"
resumo _ -> "Muitos"
```

Assim como listas podem ter elementos mais complexos que tipos primitivos, por exemplo tuplas e listas, também os casamentos de padrões aplicados a estas listas serão mais complexos.
Por exemplo, considere uma lista de três ou mais String, isto é, uma lista de lista de Char; é possível, por exemplo, selecionar a primeira letra de cada uma das três primeiras strings com a seguinte função.

```hs
iniciais :: [String] -> [Char]
iniciais [] = []
iniciais [(x:_)] = [x]
iniciais [(x:_),(y:_)] = [x,y]
iniciais ((x:_):(y:_):(z:_):_) = [x,y,z]
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
