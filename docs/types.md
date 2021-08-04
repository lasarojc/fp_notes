# Tipos

Seja a função `diasMes` a função que calcula a quantidade de dias em um mês, dado o número do mês, definida assim:

```hs
diasMes m
    | m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12 = 31
    | m == 2 = 28
    | otherwise = 30
```

O que acontece se você passar 7.2 para a função? O resultado está correto? 
Isso nos leva a perguntar como definir que uma função só é aplicável a números inteiros, do **tipo** inteiro.

Em linguagens de programação, um tipo é um nome dado a uma coleção de valores que tem um mesmo comportamento na linguagem.
Na linguagem C, por exemplo, temos os tipos `int` e `short`, ambos associados a valores inteiros mas com número de bits diferentes usados para representá-los.

Algumas linguagens são denominadas **fracamente tipadas**, o que quer dizer que o uso de tipos é relaxado e pode até ser alterado em tempo de execução.
Por exemplo, no seguinte código em Python a variável `x` assume três tipos diferentes em momentos diferentes da execução.

```py
x = "lala"
print(type(x))

x = 10
print(type(x))

x = 10.0
print(type(x))
```

```bash
<class 'str'>
<class 'int'>
<class 'float'>
```

Já a linguagem Haskell é o que chamamos de **fortemente tipada**, o que quer dizer que toda variável, constante ou função tem apenas um tipo e este sempre pode ser determinado.
Além disso, Haskell é **estaticamente tipada**, ou seja, os tipos são determinados em tempo de compilação, em oposição a linguagens que determinam o tipo durante a execução do programa, denominadas dinamicamente tipadas.
Assim como `type` em Python, Haskell tem a função `#!hs :type` para verificar o tipo de uma variável. Por exemplo:

```hs
> :type 1
1 :: Num p => p
> :type 1.0
1.0 :: Fractional p => p
> :type "Bom dia"
"Bom dia" :: [Char]
```

???sideslide "Tipos em Haskell"
    * Forte
    * Estáticos
    * Por Inferência


Finalmente, se olharmos novamente para os exemplos de funções anteriores, veremos que não foram definidos tipos em suas declarações; isto é possível porquê Haskell consegue **inferir** os tipos dos dados de forma muito eficiente. Quando você explicita tipos, o faz apenas para facilitar a leitura do código e para indicar sua intenção ao compilador que, pode-se dizer, fará a inferência e testará se você está certo.

Haskell tem um número imenso de tipos pre-definidos, organizados hierarquicamente, e que podem ser usados para definir novos tipos pelo programador.
Mas por enquanto, nos foquemos nos tipos mais simples.

## Tipos Primitivos e Operadores

###### Tipos numéricos
Como toda linguagem de programação, Haskell tem vários tipos pré-definidos para representar números, como mostra a seguinte tabela.

| Tipo |        Descrição       | Valores|
|------|------|----|
| `Int` | Inteiro de precisão finita| 4, -14, 2147483647|
| `Integer` | Inteiro de precisão arbitrária| 30414093201713378043612608166064768844377641568960512000000000000|
| `Float`  | Ponto flutuante de precisão simples | 25.132742 |
| `Double`  | Ponto flutuante de precisão dupla | 25.132741228718345 |

Dados os tipos numéricos primários, o próximo passo é ver alguns dos operadores que os manipulam.

|Operador|Operação|
|----|----|
| `+` | Adição|
| `-` | Subtração |
| `*` | Multiplicação| 
| `/` | Divisão|
| `^` | Exponenciação, e.g, `2^4 == 16`|
| `-` | Inversão de sinal, e.g, `- (-10) == 10`[^negate]|
| `negate` | Inversão de sinal, e.g, `negate (-10) == 10`[^negate]|
| `quot`| Divisão inteira, truncado pro 0, i.e., `quot a b` é igual a $\lfloor\frac{a}{b}\rfloor$ se $a>0$ e $\lceil\frac{a}{b}\rceil$ se $a<0$, e.g, `div 8 3 == 2` e `div (-8) 3 == -2`|
| `rem`| Resto da divisão inteira, tal que ```(x `quot` y)*y + (x `rem` y) == x```|
| `div`| Resto da divisão, truncado para baixo, i.e., `div a b` é igual a $\lfloor\frac{a}{b}\rfloor$, e.g, `div 8 3 == 2` e `div (-8) 3 == -3`|
| `mod`| Módulo do inteiro, tal que  ```#!hs (x `div` y)*y + (x `mod` y) == x```|

Observe que alguns destes operadores são naturalmente infixos, e.g., `+`, e outros, prefixos, e.g., `rem`.[^infix]
Contudo, ambos podem ser usados da outra forma, como mostrado nos seguintes exemplos.

[^infix]: Operadores são funções cujos nomes só contem caracteres especiais. Eles são naturalmente infixos.

[^negate]: Observe que o o `-10` está entre parênteses não porque o operador demanda, mas para deixar claro que o `-` faz parte do número.

```hs
10 + 20
(+) 10 20
quot 10 3
10 `quot` 3
```

Caso você queira ou precise especificar um tipo para um número, pode sufixá-lo com o tipo, por exemplo, `#!hs 20 :: Int` ou  `#!hs 20 :: Double`.

!!!todo "Exercícios"
    * Usar todos os operadores
    * Evidenciar diferenças entrem quot/rem e div/mod.

###### Booleanos
Finalmente, para representação de valores lógicos, Haskell define o tipo `bool`.

| Tipo | Descrição| Valores|
|------|------|----|
|`Bool` | Booleano | `True` ou `False`|

Para este tipo, temos os seguintes operadores.

|Operador|Operação|
|----|----|
| `&&` | E lógico|
| `||` | OU lógico|
| `not`| Negação|
| `==`| Igualdade| 
| `/=`| Diferença|

###### Caracteres
Para a representação de caracteres individuais, Haskell usa o tipo `Char`, e para sequências de caracteres, usa o tipo `String`, que é um "apelido" para **lista** de `Char`.[^text]
Como lista não é um tipo primitivo, ou melhor, mesmo que em Haskell listas sejam fundamentais, não as estudaremos agora.
Ainda assim, usaremos `String` em alguns exemplos, mas sem entrar em detalhes da manipulação de listas.
Exemplos dos dois tipos são apresentados na tabela a seguir.

| Tipo | Descrição| Valores|
|------|------|----|
|`Char` | Caractere | '1'; 'a'; 'B'|
|`String`| Sequência de caracteres | "Eu"; "Hello"; "Zabumba" |

Algumas funções úteis na manipulação de caracteres e strings são apresentadas na tabela.

|Operador|Operação| Exemplo |
|----|----|----|
| `++` | Concatenação | `#!hs "foo" ++ "bar" -> "foobar"`|
| `!!` | Caractere no índice| `#!hs "foo bar" !! 2 -> 'o'` |
| `take` | Substring iniciando em 0 | `#!hs take 3 "foo bar" -> "foo"` |
| `drop` | Substring começando em um índice| `#!hs drop 3 "foo bar" -> " bar"` |
| `reverse` | String ao contrário | `#!hs reverse "foo bar" -> "rab oof"` |
| `length` | Comprimento da string | `#!hs length "foo bar" -> 7` |


Nós voltaremos a falar sobre Strings uma vez que estudarmos listas.

[^text]: Haskell define também o tipo Text, uma forma mais moderna de manipular texto, mas não discutiremos Text aqui.

!!!exercise "Exercícios"
    * Defina função que retorne substring de `t` elementos começando na posição `i`
        - `#!hs minhaFuncao "entrada1" 2 2` retorna `#!hs "tr"`
    * Defina função que retorne substring com os últimos `u` elementos
        - `#!hs minhaFuncao "entrada1" 2` retorna `#!hs "a1"`
    * Defina função que receba duas strings e retorne a resultado da concatenação das substrings de `t` elementos começando na posição `i`
        - `#!hs minhaFuncao "entrada1" "entrada2" 2 2` retorna `#!hs "trtr"`


## Protótipo de funções
Agora que já conhecemos alguns tipos, podemos ver como usá-los na definição de funções.
Para fazê-lo, devemos usar a seguinte sintaxe:

```hs
nomeFuncao :: tipo_arg1 -> ... -> tipo_argN -> tipo_saida
nomeFuncao arg1 ... argN = <definicao>
```

Por exemplo, o **protótipo** da função `diaMes`, isto é, a definição dos tipos de entrada e saída da função, fica assim:

```hs
diasMes :: Int -> Int
diasMes m
    | m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12 = 31
    | m == 2 = 28
    | otherwise = 30
```


!!!exercise "Exercícios"
    * Defina o protótipo da função de conversão de Fahrenheit para Celsius `#!hs f2c x = (x - 32) /1.8`
    * Descubra a quanto 100f corresponde em Célsius

    ???example "Resolução"
        ```hs
        f2c :: Float -> Float`
        f2c x = (x - 32) /1.8
        ```

    * Defina o protótipo da função de media de dois números reais `#!hs m2n a b = (a + b)/2`
    
    ???example "Resolução"
        ```hs 
        m2n :: Float -> Float -> Float
        m2n a b = (a + b)/2
        ```


    * Defina o protótipo da soma de dois números inteiros `#!hs soma2int a b = a + b`
    * Aplique a função aos valores 2 e 3.
    * Aplique a função aos valores 2.0 e 3.0.
    
    ???example "Resolução"
        ```hs
        soma2int :: Int -> Int -> Int
        soma2int a b = a + b
        ```

    * Defina o protótipo da soma de dois números reais `#!hs soma2reais a b = a + b`
    * Aplique a função aos valores 2.0 e 3.0.
    * Aplique a função aos valores 2 e 3.
    
    ???example "Resolução"
        ```hs
        soma2reais :: Float -> Float -> Float
        ```

    * Explique a diferença de comportamento das duas últimas funções.

    ???example "Resolução"
        As funções se comportam diferentemente, sendo que a primeira mostra um erro quando aplicada a dois números reais, porquê $Z \subset  R$ mas $R \not\subset Z$.


## Definição de novos tipos

A palavra reservada `#!hs type` permite que definamos **apelidos** para tipos no Haskell, tornando o código mais legível.
Por exemplo, podemos definir um tipo `#!hs Inteiro` similar ao tipo `#!hs Int` e funções associadas ao tipo.

```hs
type Inteiro = Int

somaInteiros :: Inteiro -> Inteiro -> Inteiro
somaInteiros a b = a + b
```

O uso da função é como esperado.

```hs
*Main> somaInteiros 1 2
3
*Main> somaInteiros (1::Inteiro) (2::Inteiro)
3
```

## :type e :info

O GHC é uma ferramenta poderosa no aprendizado da linguagem Haskell por possuir uma série de comandos que permitem extrair informações sobre tipos e funções definidas.
Usando o comando `#!hs :info` (ou simplesmente `#!hs :i`), por exemplo, podemos perguntar ao ghci o que ele sabe sobre o tipo `#!hs Inteiro`, ao que será respondido que é o tipo inteiro é um apelido para `Int`, definido no arquivo scratch.hs, no meu caso.

```hs
*Main> :i Inteiro
type Inteiro :: *
type Inteiro = Int
        -- Defined at scratch.hs:82:1        
```

Já o comando `#!hs :type` (ou `#!hs :t`) pode ser usado para identificar o tipo de funções, por exemplo:

```hs
*Main> somaInteiros 1 2
3
*Main> :i somaInteiros
somaInteiros :: Inteiro -> Inteiro -> Inteiro
*Main> :t 1::Inteiro
1::Inteiro :: Inteiro
```

Estes comandos podem ser aplicados a quaisquer definições, não somente às suas. Por exemplo, podemos solicitar informações sobre o tipo `#!hs Num`, com seguinte resultado.

```hs
*Main> :i Num
type Num :: * -> Constraint
class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a
  {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}
        -- Defined in ‘GHC.Num’
instance Num Word -- Defined in ‘GHC.Num’
instance Num Integer -- Defined in ‘GHC.Num’
instance Num Int -- Defined in ‘GHC.Num’
instance Num Float -- Defined in ‘GHC.Float’
instance Num Double -- Defined in ‘GHC.Float’
```

Algumas das informações apresentadas podem ainda não fazer sentido para você, mas de forma geral podemos resumí-las como implicando que algumas operações, como `+`, `-` e `abs` se aplicam ao tipo `Num`, e que outros tipos, como `#!hs Float` e `#!hs Integer` são instâncias de `Num`.

Se aplicarmos o mesmo comando ao operador `+`, descobriremos que ele é uma função infixa por padrão (`INFIXl`), associativo à esquerda (`infixL`), e com prioridade 6.

```hs
*Main> :i (+)
type Num :: * -> Constraint
class Num a where
  (+) :: a -> a -> a
  ...
        -- Defined in ‘GHC.Num’
infixl 6 +

```

Também vemos que o estas definições são parde do GHC.Num, mas o que é o GHC.Num?

## Módulos
Como em diversas outras linguagens, Haskell usa módulos para organizar a definição de tipos e funções, colocando aquelas relacionadas no mesmo módulo.
Por padrão, o módulo **prelude**[^prelude] é carregado toda vez que executa o ghci ou compila um programa, a não ser que seja explicitamente indicado em contrário.
Este módulo contém a definição dos tipos e operadores básicos vistos anteriormente, além de muitos outros, e o GHC.Num é parte do Prelude.
Uma pequena mas interessante amostra de outros tipos incluídos:

| Nome | Definição |
|------|-----------|
|`min` | Menor de 2 elementos ordenáveis|
|`max` | Maior de 2 elementos ordenáveis|
|`Semigroup` | Uma classe em que vale a associatividade |
|`Monoid`| Monóide em que há um elemento identidade |
|`putChar`| Escreve um caractere na saida padrão |
|`putString`| Escreve uma string na saida padrão |
|`getChar`| Lê um caractere da entrada padrão |
|`getString`| Lê uma string da entrada padrão |

Estes exemplos servem para mostrar como o módulo mais básico do Haskell é diverso e como a sua biblioteca é mais diversa ainda.
Além do Prelude, centenas de outros módulos estão disponíveis na Web, de compiladores a geradores de gráficos 3D, de transformadas rápidas de Fourier a *message brokers*, em repositórios como o Hackage.[^hackage]
Contudo, é preciso ter cuidado com os módulos que baixa.
Caso você encontre um módulo que queira usar, de nome `X`, bastar baixá-lo e usar o `#!hs import`. Por exemplo, para trabalhar com números complexos, voce pode usar o módulo `#!hs Data.Complex` assim:

```hs
Prelude> import Data.Complex
Prelude Data.Complex> let x = 1.0 :+ 0.0
Prelude Data.Complex> x
1.0 :+ 0.0
```

Com esta visita rápida ao Prelude, encerramos esta introdução ao Haskell e rumamos para tópicos mais universais.
Isto é, mesmo que os tópicos vistos até agora sejam obviamente associados à programação funcional, os mesmos estão fortemente relacionados à sintaxe do Haskell.
Já nas próximas seções, veremos tópicos mais independentes, i.e., mesmo que as funções, tipos e construtos usados ainda sejam implementadas em Haskell, os conceitos por trás são mais universais.


[^hackage]: https://hackage.haskell.org/packages/browse
[^prelude]: Prelude: https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html