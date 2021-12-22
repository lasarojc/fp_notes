# Listas

Listas são peças fundamentais no desenvolvimento de programas usando o paradigma funcional, por serem estruturas de dados que permitem agregar várias informações na forma de uma coleção ordenada de elementos.
Por exemplo, `#!hs ["Eu","amo","programação","funcional"]` é a coleção de quatro strings em que o primeiro elemento é `#!hs "Eu"`, o segundo `#!hs "amo"`, o terceiro `#!hs "programação"` e o quarto e último `#!hs "funcional"`.
Já `#!hs [1::Int,2::Int,3::Int]` é uma lista de 3 elementos do tipo `#!hs Int`, onde o primeiro elemento é 1, o segundo 2, e o terceiro 3.
"Mas e as tuplas?", você pergunta, "Não são exatamente isto?"

## Listas x Tuplas
Listas tem duas particularidades que as diferenciam de tuplas.
Primeiro, enquanto as tuplas `#!hs (1::Int,2::Int,3::Int)` e `#!hs (1::Int,2::Int,3::Int,4::Int)` tem tipos diferentes, isto é, uma é uma tupla de **três** inteiros e a outra uma tupla de **quatro** inteiros, as listas `#!hs [1::Int,2::Int,3::Int]` e `#!hs [1::Int,2::Int,3::Int,4::Int]` tem exatamente o mesmo tipo, **lista de inteiros**, ou mais especificamente, `#!hs [Int]`.
Ou seja, as cardinalidades das tuplas fazem parte do seu tipo mas não das listas e portanto listas com cardinalidades diferentes mas com elementos do mesmo tipo, são do mesmo tipo.

Segundo, enquanto uma tupla pode ter elementos de tipos diferentes, todos os elementos de uma lista devem ser do mesmo tipo.
Ou seja, enquanto é possível definir `#!hs x = ("Joao", 14, True)`, não é possível definir `#!hs x = ["Joao", 14, True]`.
Aliás, outra forma de escrever a lista `#!hs [1::Int,2::Int,3::Int]`, especificando seu tipo, é `#!hs [1,2,3]::[Int]`.

É preciso observar contudo que é possível construir uma lista `#!hs [1,2,3,4,17,4.2]`, mas isto só é possível porquê existe um tipo do qual todos os elementos da lista são derivados, no caso, `#!hs Fractional`.
De fato, quando definimos este lista, Haskell automaticamente faz o boxing dos cinco primeiros valores para ponto flutuante.

```hs
> z = [1,2,3,4,17,4.2]
> z
[1.0,2.0,3.0,4.0,17.0,4.2]
> :t z
z :: Fractional a => [a]
```

Já a tupla `#!hs (1,2,3,4,17,4.2)` tem elementos com tipos diferentes.[^edicao]

[^edicao]: Foi feita uma pequena edição na saída do comando `#!hs :t` para claridade, pois o resultado real envolvia supertipos, a serem vistos mais adiante.

```hs
> :t t
t :: (Num, Num, Num, Num, Num, Fractional)
```

Uma vez diferenciadas das tuplas, estamos livres para explorar outros aspectos das listas, iniciando por como são construídas.

## Estrutura
Dado uma lista qualquer, de qualquer tipo, ela pode ser ou **vazia** ou não vazia, sendo que a lista vazia é representada em Haskell por `#!hs []`.
Listas não vazias são representadas como a concatenação do primeiro elemento da lista com uma lista com os demais elementos, usando o operador de concatenação `#!hs :`, ou **cons**.

Por exemplo, a lista dos números 1, 2 e 3 nesta ordem é construída como `#!hs 1:2:3:[]`; observe que como o 3 é o último elemento da lista, a lista que vem depois do cons é a lista vazia.

```hs
> x = 1:2:3:[]
> x
[1,2,3]
```

###### Cabeça e Cauda
Estas partes da lista, isto é o primeiro elemento e o restante, são o que chamamos de cabeça (*head*) e cauda (*tail*) da lista.
No exemplo anterior, `#!hs 1:2:3:[]`, `#!hs 1` é a cabeça e `#!hs 2:3:[]` a cauda.
Haskell inclusive define funções para recuperar estas partes de uma lista qualquer.

```hs
> head x
1
> tail x
[2,3]
```

Dado uma lista de elementos de um tipo qualquer, a cabeça desta lista é um elemento do mesmo tipo e a cauda é lista de elementos do tipo.
Logo, podemos subdividir a cauda também em uma cabeça e uma cauda, no exemplo, `#!hs 2` e `#!hs 3:[]`. 

```hs
> head (tail x)
2
> tail (tail x)
[3]
```

Podemos aplicar `#!hs head` e `#!hs tail` mais uma vez na lista, obtendo 3 e `#!hs []` como resultado.

```hs
> head (tail (tail x))
3
> tail (tail (tail x))
[]
```

Mas e se formos além? Neste caso estaríamos tentando identificar o primeiro elemento da lista, mas sendo a lista vazia, isto não é possível.
Tampouco podemos extrair a lista após a cabeça. 

```hs
> head []
*** Exception: Prelude.head: empty list
> tail []
*** Exception: Prelude.tail: empty list
```

Logo, qualquer iteração nos elementos de uma lista, geralmente especificada por uma recursão que extrai a cabeça da lista a cada passo, precisa tomar cuidado para não tentar extrair a cabeça da lista vazia, como veremos na sessão sobre recursão aplicada a listas.


###### Um pouco de açúcar sintático
A especificação manual de uma lista usando o operador cons não é muito utilizada na prática, sendo a especificação usando colchetes e vírgulas mais comum, como feito nos primeiros exemplos deste capítulo, e é inclusive como o próprio Haskell exibe as listas.
O efeito final é o mesmo, ficando para você a decisão de qual construção usar.

* `#!hs 1:[]` é igual a `#!hs [1]`
* `#!hs 2:1:[]` é igual a `#!hs [2,1]`

As duas notações podem até ser misturadas, como em `#!hs 1:2:[3,4,5]`, que é equivalente `#!hs [1,2,3,4,5]`.


###### Strings
Se o açúcar sintático dos colchetes não representa economia em termos de digitação de listas em geral, quando falamos em listas de caracteres a economia é clara e o resultado muito mais agradável. 
Isto por que para listas de caracteres, como `#!hs ['a','b','c']`, podemos escrever simplesmente `#!hs "abc"`, com exatamente o mesmo efeito, e até misturar com o uso de cons.

```hs
> "abc"
"abc"
> ['a','b','c']
"abc"
> 'a':['b','c']
"abc"
> 'a':"bc"
"abc"
```

Listas de caracteres tem este tratamento especial porquê correspondem a **strings**, as quais já estamos acostumados as trabalhar usando aspas duplas. Este tipo é tão importante que tem um apelido na linguagem, `#!hs String`, e são a forma mais básica de se lidar com texto em Haskell.[^text]

[^text]: Haskell define também o tipo `#!hs Text`, uma forma mais moderna e eficiente de se manipular texto, mas não discutiremos `#!hs Text` aqui.

!!!exercise "Exercícios"
    * Defina função que retorne substring de `t` elementos começando na posição `i`
        - `#!hs minhaFuncao "entrada1" 2 2` retorna `#!hs "tr"`
    * Defina função que retorne substring com os últimos `u` elementos
        - `#!hs minhaFuncao "entrada1" 2` retorna `#!hs "a1"`
    * Defina função que receba duas strings e retorne a resultado da concatenação das substrings de `t` elementos começando na posição `i`
        - `#!hs minhaFuncao "entrada1" "entrada2" 2 2` retorna `#!hs "trtr"`

###### [qualquer coisa]
Uma lista pode conter elementos de qualquer tipo, desde que todos os elementos sejam do mesmo tipo.
Logo, uma lista pode conter tipos primitivos, mas também tipos complexos, como tuplas e outras listas.
Vejamos alguns exemplos:

* `#!hs [1,2,3]::[Int]` - Lista de inteiros.
* `#!hs [[1,2,3]]::[[Int]]` - Lista de listas de inteiros de inteiros.
* `#!hs [[1,2,3],[],[3,4,5,6,7,8,9]]::[[Int]]` - Lista de listas de inteiros de inteiros.
* `#!hs [(1,2,3),(3,4,5)]::[(Int,Int,Int)]` - Lista de triplas de inteiros.
* `#!hs ("lala",['l','a'],'l':'ã':'o':[])` - Tripla de listas de Char.
* `#!hs ["lala",['l','a'],'l':'ã':'o':[]]` - Lista de listas de Char.
* `#!hs [True,False,True]::[Bool]` - Lista de booleanos.
* `#!hs [(4,Ouro),(5,Paus)]::[Carta]` - Lista de cartas.
* `#!hs [(1,2,3),(3)]` - Um bug.


## Funções úteis
A seguir, uma pequena amostra das funções disponíveis na biblioteca padrão do Haskell para manipulação de listas, aplicáveis dependendo do tipo da lista. Por exemplo, `#!s reverse` funciona para qualquer tipo de lista, `#!hs prod` apenas para números.

|Operador|Operação| Exemplo |
|----|----|----|
| `++` | Concatenação de listas| `#!hs > "foo" ++ "bar"` <br> `#!hs "foobar"`|
| `!!` | Elemento no índice| `#!hs >[1,2,3,4] !! 2` <br> `3` |
| `reverse` | Lista ao contrário | `#!hs >reverse [1,2,3,4] ` <br> ` [4,3,2,1]` |
| `length` | Comprimento da string | `#!hs >length "foo bar" ` <br> ` 7` |
| `last` | Último elemento da lista | `#!hs >last "foo bar"` <br> ` r`|
| `concat` | Retorna a concatenação das listas dentro de uma lista | `#!hs >concat [[1,2,3,4],[5,6,7,8]]` <br> `[1,2,3,4,5,6,7,8]` <br> `#!hs >concat [[[1,2,3]],[[4,5,6]]]` <br> `#!hs [[1,2,3],[4,5,6]]` |
| `elem`   | Verifica se o parâmetro é um elemento da lista | `#!hs >elem 'o' "foo bar"` <br> `#!hs True`|
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



## Enumeração
Para facilitar a vida dos desenvolvedores, Haskell permite a construção de listas por enumeração, bastando para isso especificar o primeiro elemento da lista, opcionalmente o segundo, e o último elemento, isto é `#!hs [<primE> {,segE}..<ultE>]`. [^optional]

[^optional]: Parâmetros especificados entre `<>` são obrigatórios e entre `{}` são opcionais.


```hs
> [11,13..23]
[11,13,15,17,19,21,23]

> [-15,-13..14]
[-15,-13,-11,-9,-7,-5,-3,-1,1,3,5,7,9,11,13]
```

Observe que Haskell determinou um passo de incremento igual a $13-11 = 2$ no primeiro exemplo e $(-13) - (-15) = 2$ no segundo exemplo, e usou estes passos para gerar as lista.

Também é possível definir um passo negativo, como no próximo exemplo.

```hs
> [11,9..0]
[11,9,7,5,3,1]
```

Como mencionado, o segundo elemento é opcional na enumeração e caso não especificado, Haskell assume que seja $1$, como no exemplo a seguir.

```hs
> [11..23]
[11,12,13,14,15,16,17,18,19,20,21,22,23]
> [3.5..10]
[3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5]
```

Contudo, não é possível omitir o segundo elemento se a intenção for gerar uma lista com valores decrescentes.

```hs
> [11..0]
[]
```

A enumeração pode ser feita para outros tipos que não sejam numéricos, bastando que exista uma relação de ordem entre os elementos para que Haskell consiga "incrementar" a cada passo.
Isso existe, por exemplo, entre os caracteres, mas possivelmente para tipos definidos pelo desenvolvedor.[^espaco]
Este ponto será revisto depois que estudarmos tipos algébricos.

[^espaco]: No exemplo, observe o espaço entre `Copas` e `..`.

```hs
> ['a'..'m']
"abcdefghijklm"

> data Naipe = Copas | Espada | Ouro | Paus deriving (Ord,Eq,Enum,Show)
> [Copas ..Ouro]
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
        > vaiEVolta 3
        [1,2,3,2,1]
        ```

    * Defina uma função que calcule o fatorial e um número n, usando `product` e listas por enumeração.

    ???example "Resolução"
        ```hs
        fatorial n = product [1..n]
        ```



## Casamento de padrões
Assim como feito com tuplas, o casamento de padrões também pode ser usado pado desmembrar os elementos de listas para processamento.
Mas diferentemente das tuplas, que tem tamanhos pré-definidos, listas de um mesmo tipo podem ter tamanhos diferentes e, por isso, o desmembramento deve ser feito de forma diferente.
Para isso, usamos o fato de que toda lista é **ou** uma lista vazia **ou** ou um elemento cabeça seguido por uma lista cauda.
Assim, se cobrir estes dois casos em uma definição por casamento de padrões, terá coberto "todos" os casos!
Vejamos um exemplo.

```hs
oQueHáNaCabeça :: (Show a) => [a] -> String
oQueHáNaCabeça [] = "Nada"
oQueHáNaCabeça (x:xs) = "Há " ++ show x
```

* Linha 1: não se preocupe com esta linha; ela apenas implica que a lista deve ser de valores convertíveis a String
* Linha 2: esta linha testa se a lista é vazia, isto é, `#!hs []` e, neste caso, retornar a string `#!hs "Nada"` como resultado.
* Linha 3: este é o caso mais interessante, pois usa um padrão que define uma lista em que `#!hs x` é a cabeça, concatenada por `#!hs :` a uma cauda `#!hs xs`, e retorna `#!hs "Há "` seguido do valor casado com `#!hs x`.

Observe que foram usados parênteses na linha 3 para especificar o padrão, e não apenas `#!hs x:xs` como seria de se esperar.
A verdade é que o padrão é `#!hs x:xs` e os parêntesis são usados apenas para impedir que Haskell primeiro avalie `#!hs oQueHáNaCabeça x` antes de avaliar o operador `#!hs :` e o seu segundo operando.
Esta é uma das idiossincrasias de Haskell com a qual você simplesmente terá que aprender a conviver para dividir listas entre cabeça e cauda em um casamento de padrões.


###### Extração de múltiplos elementos
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

Outra forma de obter exatamente o mesmo resultado, usando o operador `#!hs cons` em vez da vírgula,  seria a seguinte.

```hs
oQueHáNaLista :: (Show a) => [a] -> String
oQueHáNaLista [] = "Nada"
oQueHáNaLista (x:[]) = "Só " ++ (show x)
oQueHáNaLista (x1:x2:[]) = "Há " ++ (show x1) ++ " e " ++ (show x2)
oQueHáNaLista (x:xs) = "Há " ++ (show x) ++ " e mais um monte de coisas" 
```

Dado que strings são apenas listas de Char, estas também podem ser decompostas por casamentos de padrões.

```hs
resumo :: String -> String
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


## Compreensão
A **compreensão de listas** é uma forma de construir listas pela definição de uma regra de construção, e é muito comum nas linguagens funcionais, incluindo Haskell.


###### Definição em função de outra lista
Suponha que tenha uma lista de números e que gostaria de gerar uma nova lista em que cada valor da lista original é acrescido de 30%.
Com compreensão de listas isto pode ser feito muito facilmente usando a compreensão de listas `#!hs [e*1.3 | e <- lista]`.
Esta compreensão diz que será construída uma lista cujos elementos serão da forma `#!hs e*1.3`, onde `e` são os elementos da lista original.
Veja o exemplo de execução.

```hs
> lista = [10,20,30,40,100]
> [e*1.3 | e <- lista]
[13.0,26.0,39.0,52.0,130.0]
```

###### Sintaxe
A compreensão de listas é baseada na compreensão de conjuntos, da teoria de conjuntos da matemática.
A seguinte compreensão de conjuntos pode ser lida como o conjunto $A$ formado pela aplicação da função $f$ a todos os valores $x$ **tal que** $x$ pertence ao conjunto $C$ e para os quais valem os predicados $P_i, 1\leq i \leq n$.

$A = \{ f(x) | x \in C \land P_1(x) \land \ldots \land P_n(x)\}$

A compreensão de listas é similar

`#!hs a = [ f x | x <- c, p1 x, ...,  pn x]`

Uma diferença importante é que enquanto não há ordem nos conjuntos, há ordem nas listas e a construção é feita na ordem da lista original.

###### Listas infinitas
Assim como é possível expressar um conjunto infinito usando compreensão de conjuntos, por exemplo o conjunto dos quadrados de todos os números naturais $S = \{e^2 | e \in \mathcal{N} \}$, podemos expressar listas infinitas usando enumeração e compreensão de listas como 

`#!hs lq = [e**2 | e <- [1..]]`.

"Mas como é possível?", você me pergunta, afinal, a memória do computador é finita e portanto não poderia armazenar uma lista infinita.
Esta é uma das mágicas de Haskell, conhecida como avaliação preguiçosa, e será discutida mais adiante.
Por enquanto, basta acreditar que, desde que você não tente enumerar todos os elementos, uma lista infinita pode se representada em Haskell.
Podemos, inclusive, consultar alguns elementos da lista infinita construída acima para, por exemplo, verificar se um certo número é um quadrado perfeito!

```hs
> lq = [e**2 | e <- [1..]]
> elem 4 lq
True
> elem 16 lq
True
```

Observe, contudo, que se um elemento não estiver na lista, a função nunca retornará!

```hs
> elem 3 lq
^CInterrupted.
```

!!!exercise "Exercício"
    * Modifique o exemplo acima para limitar a quantidade de elementos que serão buscados na lista de quadrados.

    ???example "Resolução"
        ```hs
        > elem 16 (take 100 lq)
        True
        > elem 20 (take 100 lq)
        False
        > elem 64 (take 5 lq)
        False
        ```

###### Compreensão como uma iteração
Sabendo que a função `#!hs ord` do módulo `#!hs Data.Char` converte um caractere para seu valor na tabela ASCII, imagine que você queira converter uma String para uma lista dos valores ASCII correspondentes.
Isso pode ser feito trivialmente com compreensão de listas.

```hs
> import Data.Char (ord)
Data.Char> [ ord e | e <- "abcd,'dasdfa;lkqwoiur"]
[97,98,99,100,44,39,100,97,115,100,102,97,59,108,107,113,119,111,105,117,114]
```

Isto demonstra que a construção da lista pode ser usada aplicar uma função a todos os elementos de um "conjunto". No próximo exemplo, usamos esta habilidade para capitalizar de uma String.

```hs
Data.Char> import Data.Char (toUpper)
Data.Char> [ toUpper e | e <- "abcd,'dasdfa;lkqwoiur"]
"ABCD,'DASDFA;LKQWOIUR"
```

A lista resultante pode tem tipos complexos como elementos, como no exemplo seguinte, em que compreensão gera uma com tuplas com as versões minúscula e em maiúscula de cada letra encontrada na entrada.

```hs
 Data.Char> [ (toUpper e,toLower e) | e <- "abCD"]
[('A','a'),('B','b'),('C','c'),('D','d')]
```

!!!exercise "Exercício"
    * Explique `#!hs [ (e, chr ((ord e - ord 'a' + 10) `mod` 26 + (ord 'a'))) | e <- ['a'..'z']]`

    ???example "Resolução"
        Retorna uma lista de tuplas em que os primeiros elementos são letras e seus pares são letras 10 posições adiante no alfabeto, módulo 26. 

###### Predicados
Imagine agora que você queira construir uma lista com os quadrados dos números naturais múltiplos de 3 e menores que 100.
Neste caso, podemos adicionar um teste aos elementos sendo aplicados na construção da lista

```hs
> [e^2 | e <- [1..100], e `mod` 3 == 0]
[9,36,81,144,225,324,441,576,729,900,1089,1296,1521,1764,2025,2304,2601,2916,3249,3600,3969,4356,4761,5184,5625,6084,6561,7056,7569,8100,8649,9216,9801]
```

Observe que os predicados em si podem ser tão complexos quanto se queira.

```hs
 Data.Char> [e^2 | e <- [1..100], e `mod` 3 == 0, e^2 `mod` 4 == 0]
[36,144,324,576,900,1296,1764,2304,2916,3600,4356,5184,6084,7056,8100,9216]
```

!!!exercise "Exercício"
    * Usando compreensão de listas, defina uma função que gera a lista dos divisores de um número.

    ???example "Resolução"
        ```hs
        > divisores x = [e | e <- [1..x], x `mod` e == 0]
        > divisores 10
        [1,2,5,10]
        ```
    * Usando a função definida acima, defina uma função que teste se um número é primo.

    ???example "Resolução"
        ```hs
        > divisores x = [e | e <- [1..x], x `mod` e == 0]
        > primo x = divisores x == [1,x]
        > primo 7
        True
        > primo 45
        False
        ```


###### Múltiplos geradores
Uma compreensão de listas pode ter mais de um gerador (`#! <-`), o que faz com que todas as combinações dos elementos gerados sejam aplicadas à função.
Por exemplo, a seguinte compreensão combina todos os números de 1 a 4 com todos os números de 1 a 4 na construção de uma tupla de dois inteiros.

```hs
> [(x,y) | x <- [1..4], y <- [1..4]]
[(1,1),(1,2),(1,3),(1,4),(2,1),(2,2),(2,3),(2,4),(3,1),(3,2),(3,3),(3,4),(4,1),(4,2),(4,3),(4,4)]
```

Veja que predicados podem ser normalmente aplicados a múltiplos geradores, como no seguinte exemplo, em que somente as combinações onde $x,y$ são usadas.

```hs
> [(x,y) | x <- [1..4], y <- [1..4], x < y]
[(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]
```

É importante observar que a ordem dos geradores altera a ordem dos elementos da lista gerada, pois para cada elemento gerado pelo primeiro gerador, será combinado a cada elemento gerado pelo segundo.

```hs
> [ (x,y) | x <- [1..4], y <- ['a'..'d']]
[(1,'a'),(1,'b'),(1,'c'),(1,'d'),(2,'a'),(2,'b'),(2,'c'),(2,'d'),(3,'a'),(3,'b'),(3,'c'),(3,'d'),(4,'a'),(4,'b'),(4,'c'),(4,'d')]
> [ (x,y) |  y <- ['a'..'d'], x <- [1..4]]
[(1,'a'),(2,'a'),(3,'a'),(4,'a'),(1,'b'),(2,'b'),(3,'b'),(4,'b'),(1,'c'),(2,'c'),(3,'c'),(4,'c'),(1,'d'),(2,'d'),(3,'d'),(4,'d')]
```

Além disso, é possível definir um gerador em termos dos geradores anteriores. Por exemplo

```hs
> [ (x,y) | x <- [1..4], y <- [1..x]]
[(1,1),(2,1),(2,2),(3,1),(3,2),(3,3),(4,1),(4,2),(4,3),(4,4)]
```

Este construto é deveras poderoso, pois geradores podem ser aplicados eles próprios a compreensão de listas.
Outra possibilidade é a aplicação recursiva, como no seguinte código.

```hs
partitions [] = [[]]
partitions (x:xs) = [ x:e | e <- partitions xs] ++ partitions xs
```

!!!exercise "Exercício"
    * Explique o que a função acima faz, com um exemplo.
    * Dado uma lista `l`, calcule todas as permutações de seus elementos.

    !!!example "Resolução"
        ```hs
        permutações :: [a] -> [[a]]
        permutações [] -> [[]]
        permutações (h:t) = [take n permCauda ++ [h] ++ drop n permCauda | n <- [0..length t], permCauda <- permutações t]
        ```

