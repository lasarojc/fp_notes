# Listas

Vamos agora estudar listas, peças fundamentais no desenvolvimento de programas usando o paradigma funcional, por serem estruturas de dados que permitem agregar várias informações na forma de uma coleção ordenada de elementos.
Por exemplo, `#!hs ["Eu","amo","programação","funcional"]` é a coleção de quatro strings em que o primeiro elemento é `#!hs "Eu"`, o segundo `#!hs "amo"`, o terceiro `#!hs "programação"` e o quarto e último `#!hs "funcional"`.
Já `#!hs [1::Int,2::Int,3::Int]` é uma lista de 3 elementos do tipo `#!hs Int`, onde o primeiro elemento é 1, o segundo 2, e o terceiro 3.
"Mas e as tuplas?", você pergunta, "Não são exatamente isto?"

###### Listas x Tuplas
Listas tem duas particularidades que as diferenciam de tuplas.
Primeiro, enquanto as tuplas `#!hs (1::Int,2::Int,3::Int)` e `#!hs (1::Int,2::Int,3::Int,4::Int)` tem tipos diferentes, isto é, uma é uma tupla de **três** inteiros e a outra uma tupla de **quatro** inteiros, as listas `#!hs [1::Int,2::Int,3::Int]` e `#!hs [1::Int,2::Int,3::Int,4::Int]` tem exatamente o mesmo tipo, lista de inteiros, ou mais especificamente, `#!hs [Int]`. Ou seja, listas com cardinalidades diferentes, mas com elementos do mesmo tipo, são do mesmo tipo.

Segundo, enquanto uma tupla pode ter elementos de tipos diferentes, todos os elementos de uma lista devem ser do mesmo tipo.
Ou seja, enquanto é possível definir `#!hs x = ("Joao", 14, True")`, não é possível definir `#!hs x = ["Joao", 14, True"`.
É preciso observer contudo que é possível fazer `#!hs [1,2,3,4,17,4.2]`, com 2 sendo um número inteiro e 4.2 sendo ponto flutuante.
Isto por que quando esta lista é definida, o Haskell procura um tipo que de onde todos os elementos da lista sejam derivados, no caso, `#!hs Fractional`.
De fato, quando defino este lista, o Haskell automaticamente faz o boxing dos cinco primeiros valores para ponto flutuante.

```hs
*Main> z = [1,2,3,4,17,4.2]
*Main> z
[1.0,2.0,3.0,4.0,17.0,4.2]
*Main> :t z
z :: Fractional a => [a]
```

Já a tupla `#!hs (1,2,3,4,17,4.2)` tem elementos com tipos diferentes.[^edicao]

[^edicao]: Foi feita uma pequena edição na saída do comando `#!hs :t` para claridade, pois o resultado real envolvia supertipos, a serem vistos mais adiante.

```hs
Prelude> :t t
t :: (Num, Num, Num, Num, Num, Fractional)
```

Uma vez diferenciadas das tuplas, estamos livres para explorar outros aspectos das listas, iniciando por como são construídas.

## Estrutura
Dado uma lista qualquer, de qualquer tipo, ela pode ser ou **vazia** ou não vazia, sendo que a lista vazia é construída em Haskell como `#!hs []`.
Listas não vazias são representadas como a concatenação do primeiro elemento da lista com uma lista com os demais elementos, usando o operador de concatenação `#!hs :`, ou **cons**.

Por exemplo, a lista dos números 1, 2 e 3 nesta ordem é construída como `#!hs 1:2:3:[]`; observe que como o 3 é o último elemento da lista, a lista que vem depois do cons, com os demais elementos, é a lista vazia.

```hs
Prelude> x = 1:2:3:[]
Prelude> x
[1,2,3]
```

###### Cabeça e Calda
Esta divisão de uma lista entre primeiro elemento e restante é o que chamamos de cabeça (*head*) e calda (*tail*) da lista.
No exemplo anterior, `#!hs 1:2:3:[]`, 1 é a cabeça e `#!hs 2:3:[]` a calda.
Haskell inclusive define funções para recuperar estas partes de uma lista qualquer.

```hs
Prelude> head x
1
Prelude> tail x
[2,3]
```

A cabeça de uma lista de elementos de um tipo $a$ qualquer é um elemento do tipo $a$.
Já a calda desta lista é também uma lista de elementos do tipo $a$.
Logo, podemos subdividí-la também em uma cabeça e uma cauda, no exemplo, 2 e `#!hs 3:[]`. 

```hs
Prelude> head (tail x)
2
Prelude> tail (tail x)
[3]
```

Podemos aplicar `#!hs head` e `#!hs tail` mais uma vez na lista, obtendo 3 e `#!hs []` como resultado.

```hs
Prelude> head (tail (tail x))
3
Prelude> tail (tail (tail x))
[]
```

Mas e se formos além? Neste caso estaríamos tentando identificar o primeiro elemento da lista, mas sendo a lista vazia, isto não é possível.
Tampouco podemos extrair a lista após a cabeça. 

```hs
Prelude> head []
*** Exception: Prelude.head: empty list
Prelude> tail []
*** Exception: Prelude.tail: empty list
```

Logo, qualquer iteração nos elementos de uma lista, geralmente especificada por uma recursão que extrai a cabeça da lista a cada passo, precisa tomar cuidado para não tentar extrair a cabeça da lista vazia, como veremos na sessão sobre recursão aplicada a listas.


###### Um pouco de açúcar sintático
A especificação manual de uma lista usando o operador cons não é muito utilizada na prática, sendo a especificação usando colchetes e vírgulas mais comum, como feito nos primeiros exemplos deste capítulo, e é inclusive como o próprio Haskell exibe as listas.
O efeito final é o mesmo, ficando para você a decisão qual construção usar.

* `#!hs 1:[]` é igual a `#!hs [1]`
* `#!hs 2:1:[]` é igual a `#!hs [2,1]`

###### Strings
Se o açúcar sintático dos colchetes representa nenhuma economia em termos de digitação de listas em geral, quando falamos em listas de caracteres a economia é clara e o resultado muito mais agradável. Isto por que para listas de caracteres, como `#!hs ['a','b','c']`, podemos escrever simplesmente `#!hs "abc"`, com exatamente o mesmo efeito.

```hs
Prelude> minhaLista = ['a','b','c']
Prelude> minhaLista
"abc"
Prelude> minhaOutraLista = "abc"
Prelude> minhaOutraLista 
"abc"
Prelude> minhaLista == minhaOutraLista 
True
```

## Funções úteis
as de string
outras

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

## `..`
enumerações

umADez = [1..10]