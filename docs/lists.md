# Listas

Vamos agora estudar listas, peças fundamentais no desenvolvimento de programas usando o paradigma funcional, por serem estruturas de dados que permitem agregar várias informações na forma de uma coleção ordenada de elementos.
Por exemplo, `#!hs ["Eu","amo","programação","funcional"]` é a coleção de quatro strings em que o primeiro elemento é `#!hs "Eu"`, o segundo `#!hs "amo"`, o terceiro `#!hs "programação"` e o quarto e último `#!hs "funcional"`.
Já `#!hs [1::Int,2::Int,3::Int]` é uma lista de 3 elementos do tipo `#!hs Int`, onde o primeiro elemento é 1, o segundo 2, e o terceiro 3.
"Mas e as tuplas?", você pergunta, "Não são exatamente isto?"

###### Listas x Tuplas
Listas tem duas particularidades que as diferenciam de tuplas.
Primeiro, enquanto as tuplas `#!hs (1::Int,2::Int,3::Int)` e `#!hs (1::Int,2::Int,3::Int,4::Int)` tem tipos diferentes, isto é, uma é uma tupla de **três** inteiros e a outra uma tupla de **quatro** inteiros, as listas `#!hs [1::Int,2::Int,3::Int]` e `#!hs [1::Int,2::Int,3::Int,4::Int]`tem exatamente o mesmo tipo, lista de inteiros, ou mais especificamente, `#!hs [Int]`.
Ou seja, listas com cardinalidades diferentes, mas com elementos do mesmo tipo, são do mesmo tipo.
Aliás, outra forma de escrever estas listas, enquanto especificando seus tipos, é `#!hs [1,2,3]::[Int]` e `#!hs [1,2,3,4]::[Int]`. 

Segundo, enquanto uma tupla pode ter elementos de tipos diferentes, todos os elementos de uma lista devem ser do mesmo tipo.
Ou seja, enquanto é possível definir `#!hs x = ("Joao", 14, True")`, não é possível definir `#!hs x = ["Joao", 14, True"]`.
É preciso observer contudo que é possível construir uma lista `#!hs [1,2,3,4,17,4.2]`, mas mas isto só é possível porquê existe um tipo do qual todos os elementos da lista são derivados, no caso, `#!hs Fractional`.
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

As duas notações podem até ser misturadas, como em `#!hs 1:2:[3,4,5]`, que é equivalente `#!hs [1,2,3,4,5]`.


###### Strings
Se o açúcar sintático dos colchetes não representa economia em termos de digitação de listas em geral, quando falamos em listas de caracteres a economia é clara e o resultado muito mais agradável. 
Isto por que para listas de caracteres, como `#!hs ['a','b','c']`, podemos escrever simplesmente `#!hs "abc"`, com exatamente o mesmo efeito, e até misturar com o uso de cons.

```hs
Prelude> "abc"
"abc"
Prelude> ['a','b','c']
"abc"
Prelude> 'a':['b','c']
"abc"
Prelude> 'a':"bc"
"abc"
```

###### [qualquer coisa]
Uma lista pode conter elementos de qualquer tipo, desde que todos os elementos sejam do mesmo tipo.
Logo, uma lista pode conter tipos primitivos, mas também tipos complexos, como tuplas e outras listas.
Vejamos alguns exemplos:

* `#!hs [1,2,3]::[Int]` - Lista de inteiros.
* `#!hs [[1,2,3]]::[[Int]]` - Lista de listas de inteiros de inteiros.
* `#!hs [[1,2,3],[],[3,4,5,6,7,8,9]]::[[Int]]` - Lista de listas de inteiros de inteiros.
* `#!hs [(1,2,3),(3,4,5)]::[(Int,Int,Int)]` - Lista de triplas de inteiros.
* `#!hs ("lala",['l','a'],'l':'ã':'o':[])` - Tripla de listas de Char.
* `#!hs [True,False,True]::[Bool]` - Lista de booleanos.
* `#!hs [(4,Ouro),(5,Paus)]::[Carta]` - Lista de cartas.
* `#!hs [(1,2,3),(3)]` - Um bug


## Funções úteis
Em uma seção anterior, apresentamos algumas funções como úteis na manipulação de String.
Na verdade, todas aquelas funções são definidas em termos de listas, e por isso as revisitaremos aqui, juntamente com mais algumas.


|Operador|Operação| Exemplo |
|----|----|----|
| `++` | Concatenação de listas| `#!hs > "foo" ++ "bar"` <br> `#!hs "foobar"`|
| `!!` | Elemento no índice| `#!hs >[1,2,3,4] !! 2 -> 3` |
| `reverse` | Lista ao contrário | `#!hs >reverse [1,2,3,4] ` <br> ` [4,3,2,1]` |
| `length` | Comprimento da string | `#!hs >length "foo bar" ` <br> ` 7` |
| `last` | Último elemento da lista | `#!hs >last "foo bar"` <br> ` r`|
| `concat` | Retorna a concatenação das listas dentro de uma lista | `#!hs >concat [[1,2,3,4],[5,6,7,8]]` <br> `[1,2,3,4,5,6,7,8]` <br> `#!hs >concat [[[1,2,3]],[[4,5,6]]]` <br> `#!hs [[1,2,3],[4,5,6]]` |
| `elem`   | Verifica se o parâmetro é um elemento da lista | `#!hs >elem o "foo bar"` <br> `#!hs True`|
| `null`   | Verifica se a lista é vazia | `#!hs >null ""` <br> `True`<br> `#!hs >null []` <br> `True` <br> `#!hs >null [1,2]` <br> `#!hs False`|
| `replicate`   | Constrói uma lista pela replicação de um elemento | `#!hs >replicate 4 (1,2)` <br> `#!hs [(1,2),(1,2),(1,2),(1,2)]`|
| `take` | Sublista iniciando em 0 | `#!hs >take 3 1:2:3:4:5:[] ` <br> `#!hs [1,2,3]` |
| `drop` | Sublista começando em um índice| `#!hs >drop 3 ['f','o','o',' ','b','a','r'] ` <br> `#!hs " bar"` |
| `takeWhile` | Sublista iniciando em 0 e até o primeiro elemento que não satisfaz ao critério, exclusive| `#!hs >takeWhile (<4) [1,2,3,4,5,6]` <br> `#!hs [1,2,3]` |
| `dropWhile` | Sublista começando no primeiro elemento que não satisfaz ao critério, inclusive| `#!hs >dropWhile (<4) [1,2,3,4,5,6]` <br> `#!hs 4,5,6` |
| `splitAt` | Dupla das sublistas geradas pela divisão no índice especificado| `#!hs >splitAt 3 [1,2,3,4,5,6]` <br> `#!hs ([1,2,3],[4,5,6])` |
| `zip` | Lista de pares com os elementos das duas listas passadas como parâmetro| `#!hs >zip [1,2,3] [4,5,6]` <br> `#!hs [(1,4),(2,5),(3,6)]` |
| `sum` | Somatório dos elementos da lista| `#!hs >sum [1,2,3,4,5,6]` <br> `#!hs 16` |
| `product` | Produtório dos elementos da lista| `#!hs >product [1,2,3,4,5,6]` <br> `#!hs 720` |
| `maximum` | Maior dos elemento lista| `#!hs >maximum [1,2,3,4,5,6]` <br> `#!hs 6` |
| `minimum` | Menor dos elementos da lista| `#!hs >minimum [1,2,3,4,5,6]` <br> `#!hs 1` |


!!!exercise "Exercícios"
    * Zip de listas de tipos diferentes.
    * Mínimo e máximo de lista de tuplas.


## Enumeração
Para facilitar a vida dos desenvolvedores, o Haskell permite a construção de listas por enumeração, bastando para isso o especificar o primeiro elemento da lista, opcionalmente o segundo, e o último elemento.
Por exemplo
```hs
Prelude> [11,13..23]
[11,13,15,17,19,21,23]

Prelude> [-15,-13..14]
[-15,-13,-11,-9,-7,-5,-3,-1,1,3,5,7,9,11,13]
```

Observe que o Haskell determinou um passo de incremento igual a $13-11 = 2$ no primeiro exemplo e $-15 - -13 = 2$ no segundo exemplo, e usou estes passos para gerar as lista.

Também é possível definir um passo negativo, como no próximo exemplo.

```hs
Prelude> [11,9..0]
[11,9,7,5,3,1]
```

Como mencionado, o segundo elemento é opcional na enumeração e caso não especificado, o Haskell assume que seja $1$, como no exemplo a seguir.

```hs
Prelude> [11..23]
[11,12,13,14,15,16,17,18,19,20,21,22,23]
Prelude> [3.5..10]
[3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5]
```

Contudo, não é possível omitir o segundo elemento se a intenção gerar uma lista com valores decrescentes.

```hs
Prelude> [11..0]
[]
```

A enumeração pode ser feita para outros tipos que não sejam numéricos, bastando que exista uma relação de ordem entre os elementos para que o Haskell consiga "incrementar" a cada passo.
Isso existe, por exemplo, entre os caracteres, mas também para tipos definidos pelo desenvolvedor.[^espaco]

[^espaco]: No exemplo, observe o espaço entre `Copas` e `..`.

```hs
Prelude> ['a'..'m']
"abcdefghijklm"

Prelude> data Naipe = Copas | Espada | Ouro | Paus deriving (Ord,Eq,Enum,Show)
Prelude> [Copas ..Ouro]
[Copas,Espada,Ouro]
```


!!!exercise "Exercício"
    * Defina uma função que dado um número inteiro x, gere uma lista de 1 a x e de volta a 1.

    ???example "Resolução"
        ```hs
        vaiEVolta n = lista ++ drop 1 (reverse lista)
            where lista = [1..n]
        ```

        ```hs
        *Main> vaiEVolta 3
        [1,2,3,2,1]
        ```
    * Defina uma função que dado uma String, verifique se ela é um palíndromo.

    ???example "Resolução"
        ```hs
        éPalíndromo s = s == reverse s
        ```

        ```hs
        *Main> éPalíndromo "aba"
        True
        *Main> éPalíndromo "abac"
        False
        ```

    * Defina uma função que calcule o fatorial e um número n, usando `product` e listas por enumeração.

    ???example "Resolução"
    ```hs
    fatorial n = product [1..n]
    ```