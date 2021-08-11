# Listas
Além das tuplas, discutidas anteriormente, listas também são uma forma de agregar múltiplas informações em um único valor.
Mas o que é uma lista? É uma coleção ordenada de elementos, por exemplo, `#!hs ["Eu","amo","programação","funcional"]` é coleção de quatro strings, em que o primeiro elemento é "Eu", o segundo "amo", e assim por diante. Já `#!hs String` e `#!hs [1,2,3,17,12]` é uma lista de cinco elementos do tipo `#!hs Num`, que é um supertipo de números inteiros e de ponto flutuante.

Listas tem duas particularidades que as diferenciam de tuplas.
Primeiro, enquanto as tuplas `#!hs (1::Int,2::Int,3::Int)` e `#!hs (1::Int,2::Int,3::Int,4::Int)` tem tipos diferentes, isto é, uma é uma tupla de **três** inteiros e a outra uma tupla de **quatro** inteiros, as listas `#!hs [1::Int,2::Int,3::Int]` e `#!hs [1::Int,2::Int,3::Int,4::Int]` tem exatamente o mesmo tipo, lista de inteiro, ou mais especificamente, `#!hs [Int]`.
Ou seja, listas com cardinalidades diferentes, mas com elementos do mesmo tipo, são do mesmo tipo.

Segundo, enquanto uma tupla pode ter elementos de tipos diferentes, todos os elementos de uma lista devem ser do mesmo tipo.
Por exemplo, `#!hs [1,2,3,4,17,4.2]` é uma lista de `#!hs Fractional`, que é um supertipo dos números de ponto flutuante.
De fato, quando defino este lista, o Haskell automaticamente faz o boxing dos cinco primeiros para ponto flutuante.

```hs
Prelude> z = [1,2,3,4,17,4.2]
Prelude> z
[1.0,2.0,3.0,4.0,17.0,4.2]
```

###### Strings
Açúcar sintático par `[Char]`

###### [] e :

* `[]`
    * Lista vazia
    * `:t []` - [a]
* `:`
    * `1:[]` é igual a `[1]`
    * `2:1:[]` é igual a `[2,1]`

###### Casamento de padrões
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

## `..`
enumerações

umADez = [1..10]