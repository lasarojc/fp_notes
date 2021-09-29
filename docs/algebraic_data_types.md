# Tipos de dados algébricos
Em seções anteriores, vimos que Haskell define diversos [tipos](types.md) de dados, como inteiros, booleanos e strings.

Embora possamos usar os tipos básicos para resolver problemas, pode ser mais fácil usar um vocabulário específico do problema, por exemplo data, nome, direção, etc.
Neste caso, podemos criar ["apelidos"](types.md) para tipos básicos, usando `#!hs type`.
Mas Haskell é mais poderoso que isso e permite que defina tipos novos a partir do zero.[^davesands]
Por exemplo, lembre-se do problemas discutidos que lidavam com cartas de baralho e como usamos uma dupla de String e Int para representar seus naipes e valores.
Com `#!hs data` é possível fazer algo melhor.

[^davesands]: Esta aula é fortemente inspirada na video aula [Enumeration Types, Show](https://youtu.be/pfQVVbKyNwE) de Dave Sands.

## `#!hs data`
Com a palavra reservada `#!hs data` é possível definir um novo tipo de dados (em inglês, *data*).
O seguinte uso define um tipo para os naipes das cartas de um baralho.


```hs
data Naipe = Copas | Espadas | Ouro | Paus
```

Pela equação seguinte, definimos o tipo de dados `#!hs Naipe` (lado esquerdo) como sendo igual a **um dos** valores do lado direito, dado que o lado direito é uma disjunção.
É comum dizer que `#!hs Naipe` é uma **enumeração** dos valores à direita da equação.

Uma vez definido o tipo, podemos perguntar ao Haskell como ele é interpretado.

```hs
> data Naipe = Copas | Espadas | Ouro | Paus 
> :i Naipe
type Naipe :: *
data Naipe = Copas | Espadas | Ouro | Paus
        -- Defined at <interactive>:1:1
> :i Copas
type Naipe :: *
data Naipe = Copas | ...
        -- Defined at <interactive>:1:14
> :t Copas
Copas :: Naipe
```

Você também já pode usar o tipo em sua definições, por exemplo:

```hs
corDoNaipe :: Naipe -> String
corDoNaipe Copas = "Vermelho"
corDoNaipe Ouro = "Vermelho"
corDoNaipe Paus = "Preto"
corDoNaipe Espada = "Preto"
```

Ou, equivalentemente, no seguinte exemplo.

```hs
corDoNaipe :: Naipe -> String
corDoNaipe n = case n of 
                  Copas -> "Vermelho"
                  Ouro -> "Vermelho"
                  Paus -> "Preto"
                  Espada -> "Preto"
```

Mas e o seguinte código?

```hs
corDoNaipe'' :: Naipe -> String
corDoNaipe'' n
    | n == Copas = "Vermelho"
    | n == Ouro = "Vermelho"
    | n == Paus = "Preto"
    | n == Espada = "Preto"
```

Se testá-lo, verá que não funciona. Um efeito semelhante é observado quando fazemos algo mais simples ainda.

```hs
Prelude> data Naipe = Copas | Espada | Ouro | Paus
Prelude> Copas == Copas

<interactive>:2:1: error:
    • No instance for (Eq Naipe) arising from a use of ‘==’
    • In the expression: Copas == Copas
      In an equation for ‘it’: it = Copas == Copas
```

O problema aqui é que Haskell não sabe como testar se dois naipes são iguais!
Agora teste o seguinte.

```hs
Prelude> True == True
True
```

Qual a diferença?

## Classes de tipos

Observe que o tipo `#!hs Bool` foi definido da mesma forma, exceto por algumas informações extra que aparecem quando o tipo é descrito.

```hs
Prelude> :i Bool
type Bool :: *
data Bool = False | True
        -- Defined in ‘GHC.Types’
instance Eq Bool -- Defined in ‘GHC.Classes’
instance Ord Bool -- Defined in ‘GHC.Classes’
instance Enum Bool -- Defined in ‘GHC.Enum’
instance Show Bool -- Defined in ‘GHC.Show’
instance Read Bool -- Defined in ‘GHC.Read’
instance Bounded Bool -- Defined in ‘GHC.Enum’
```

Mas o que são estas informações extra? Façamos um teste, simplesmente avaliando um dos valores de `#!hs Bool` e de `#!hs Naipe`.

```hs
> True
True
> Copas

<interactive>:9:1: error:
    • No instance for (Show Naipe) arising from a use of ‘print’
    • In a stmt of an interactive GHCi command: print it
```

O erro aparece porquê quando a avaliação é feita, o GHCi tenta imprimir o resultado na tela, e para imprimir o resultado ele tenta obter sua representação como `#!hs String` usando a função `#!hs show`.

```hs
> show True
"True"
> show Copas

<interactive>:11:1: error:
    • No instance for (Show Naipe) arising from a use of ‘show’
    • In the expression: show Copas
      In an equation for ‘it’: it = show Copas
```

No caso do valor booleano, a função funciona, mas no caso do naipe não!

Volte no trecho acima onde descrevemos o valor verdadeiro.
Uma das diferenças para o naipe era a presença da linha `#!hs instance Show Bool -- Defined in ‘GHC.Show’` que basicamente dizia que `#!hs Bool` faz parte da **classe de tipos** `#!hs Show`, a classe dos tipos que podem ser passados como parâmetro para função `#!hs show`.
Há várias classes de tipo em Haskell, e você ainda pode criar as suas próprias.

###### Show
No caso do exemplo anterior, é possível incluir o tipo naipe na classe em questão de uma forma manual definindo uma série de funções necessárias ao `#!hs Show` (`#!hs :i Show`).
Mas há um atalho que diz que todas as funções necessárias devem ser construídas segundo um padrão, que neste caso basicamente quer dizer que o valor em `#!hs String` é simplesmente o texto usado na declaração.
Para fazer isso, basta modificar a definição da seguinte forma.

```hs
data Naipe = Copas | Espadas | Ouro | Paus
    deriving (Show)
```

Assim, podemos refazer as consultas e testes feitas anteriormente.

```hs
Prelude> data Naipe = Copas | Espadas | Ouro | Paus deriving (Show)
Prelude> :i Naipe
type Naipe :: *
data Naipe = Copas | Espadas | Ouro | Paus
        -- Defined at <interactive>:12:1
instance [safe] Show Naipe -- Defined at <interactive>:12:54
Prelude> show Copas
"Copas"
Prelude> Copas
Copas
```

###### Eq
Assim como `#!hs Show`, `#!hs Eq` é uma classe de tipo que define que todos os membros da class devem ter definidas algumas operações, em específico, os operadores `#!hs (==)` e `#!hs (/=)`, como 
`#!hs :i Eq` mostra:

```hs
> :i Eq
type Eq :: * -> Constraint
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  {-# MINIMAL (==) | (/=) #-}
  ...
```

###### Ord
Por sua vez,  `#!hs Ord` é uma classe de tipo que define capacidades de comparação entre elementos de um tipo, como `#!hs :i Ord` mostra:

```hs
> :i Ord
type Ord :: * -> Constraint
class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  max :: a -> a -> a
  min :: a -> a -> a
  {-# MINIMAL compare | (<=) #-}
  ...
```

Observe que para que um tipo pertencer a `#!hs Ord` ele também deve pertencer a `#!hs Eq`.

###### Enum
Finalmente, `#!hs Enum` define a capacidade de, dado um valor de um certo tipo, determinar antecessores e sucessores, bem como construir listas por enumeração.
```hs
> :i Enum
type Enum :: * -> Constraint
class Enum a where
  succ :: a -> a
  pred :: a -> a
  toEnum :: Int -> a
  fromEnum :: a -> Int
  enumFrom :: a -> [a]
  enumFromThen :: a -> a -> [a]
  enumFromTo :: a -> a -> [a]
  enumFromThenTo :: a -> a -> a -> [a]
  {-# MINIMAL toEnum, fromEnum #-}
  ...
```

###### Read

???todo "TODO
     `#!hs read "100" :: Int`

###### Definição completa
Definindo o tipo para o naipe com todas estas classes de tipo, teremos um tipo bem interessante, que pode ser impresso na tela, comparado e usado para gerar listas por enumeração.

```hs
> data Naipe = Copas | Espadas | Ouro | Paus deriving (Show,Eq,Enum,Ord)
> [Copas .. Paus]
[Copas,Espadas,Ouro,Paus]
> pred Ouro
Espadas
```

Contudo, há limitações para o que se pode fazer com esta definição, como demonstrado pelo próximo exemplo.

```hs
> pred Copas
*** Exception: pred{Naipe}: tried to take `pred' of first tag in enumeration
CallStack (from HasCallStack):
  error, called at <interactive>:9:62 in interactive:Ghci1
```

## Tipos mais complexos
Mas e se quisermos definir um tipo para representar o valor de uma carta?

```hs
data Valor = Ás | Número1 | Número2 | Número3 | Número4 | Número5 | Número6 | Número7 | Número8 | Número9 | Número10 | Valete | Dama | Rei
  deriving (Eq,Show,Ord,Enum)
```

Com esta definição é possível, por exemplo, comparar os valores das cartas.
Mas, convenhamos, é uma definição horrível.
Haskell to the rescue!
É possível definir um valor que seja baseado em outro tipo, como no seguinte exemplo.

```hs
data Valor = Ás | Número Int | Valete | Dama | Rei
```

`#!hs Número Int` define que qualquer combinação de `#!hs Número`, denominado o **construtor**, combinado com um valor do tipo `#hs Int`, é um valor do tipo `#!hs Valor`.

```hs
> data Valor = Ás | Número Int | Valete | Dama | Rei deriving (Eq,Show, Ord)
> as = Ás
> valete = Valete
> nove = Número 9
> as
Ás
> valete
Valete
> nove
Número 9
```

Assim como `#!hs Naipe` pode ser usado em um casamento de padrões, também a definição de `#!hs Valor` pode, como no seguinte exemplo.

```hs
éFigura :: Valor -> Bool
éFigura (Número _) -> False
éFigura Ás -> True  -- Esta definição é desnecessária e usada só pra demonstração.
éFigura _ -> True
```


###### Ord e Eq

Como na definição de `#!hs Valor` tanto `#!hs Ord` e `#!hs Eq` foram usados, Haskell deve ser capaz de comparar todas as possibilidades de valores.
Assim, por exemplo, `#!hs Ás` é menor que qualquer número, que é menor que qualquer outra figura.
Também quaisquer dois números podem ser comparados, sendo resultado determinado pela comparação dos respectivos inteiros.

```hs
> data Valor = Ás | Número Int | Valete | Dama | Rei      deriving (Eq,Show,Ord)
> Número 3 < Número 4
True
> Número 3 == Número 4
False
> Ás < Número 10
True
> Ás < Rei
True
```

Observe que a definição não inclui `#!hs Enum`, para entender porquê, tente determinar qual seria o sucessor de `#!hs Ás` e o antecessor de `#!hs Valete`.
Como não é possível determinar estes valores, o compilador nem permite que `#!hs Enum` seja usado.

```hs
> data Valor = Ás | Número Int | Valete | Dama | Rei      deriving (Eq,Show,Ord,Enum)

<interactive>:21:79: error:
    • Can't make a derived instance of ‘Enum Valor’:
        ‘Valor’ must be an enumeration type
        (an enumeration consists of one or more nullary, non-GADT constructors)
    • In the data declaration for ‘Valor’
```


## Tipos mais complexos ainda
Combinemos agora os tipos `#!hs Naipe` e `#!hs Valor` em um único tipo que representa uma carta de baralho.
A instanciação é feita usando-se o construtor.


```hs
> data Valor = Ás | Número Int | Valete | Dama | Rei      deriving (Eq,Show,Ord)
> data Naipe = Copas | Espadas | Ouro | Paus              deriving (Show,Eq,Enum,Ord)
> data CartaT = Carta Naipe Valor                         deriving (Eq,Show,Ord)

> Carta Paus Rei
Carta Paus Rei
> Carta Paus (Número 3)
Carta Paus (Número 3)
> Carta Paus (Número 3) < Carta Paus Rei
True
> Carta Paus (Número 3) < Carta Ouro Rei
False

> k = Carta Paus Rei
> k
Carta Paus Rei
> :t k
k :: CartaT
> :i k
k :: CartaT 	-- Defined at <interactive>:28:1
```

Veja que do lado esquerdo da equação temos a definição de um tipo `#!hs CartaT` e do lado direito temos a definição de um construtor `#!hs Carta` para o tipo `#!hs CartaT`.
É possível solicitar mais informações do Haskell tanto sobre o construtor quanto o tipo, mas nem todas as solicitações fazem sentido.
Quanto ao tipo, `#!hs CartaT`, é possível pedir informações sobre a definição, mas não o tipo da definição.

```hs
> :i CartaT
type CartaT :: *
data CartaT = Carta Naipe Valor
  	-- Defined at <interactive>:26:1
instance [safe] Ord CartaT -- Defined at <interactive>:26:75
instance [safe] Show CartaT -- Defined at <interactive>:26:70
instance [safe] Eq CartaT -- Defined at <interactive>:26:67

> :t CartaT
<interactive>:1:1: error:
    • Data constructor not in scope: CartaT
    • Perhaps you meant ‘Carta’ (line 26)
```

Quanto ao construtor, `#!hs Carta`, é possível perguntar as duas coisas.

```hs
> :i Carta
type CartaT :: *
data CartaT = Carta Naipe Valor
  	-- Defined at <interactive>:26:15

> :t Carta
Carta :: Naipe -> Valor -> CartaT
```

Veja que quanto perguntamos o tipo do construtor, a resposta é que é uma função que recebe `#!hs Naipe` e `#!hs Valor` e que retorna uma instância do tipo `#!hs CartaT`.

###### Punning
Uma vez que esteja claro que tipo e construtores são coisas diferentes, é preciso dizer que seus contextos geralmente são diferentes e que, por isso, é possível que ambos tenham o mesmo nome.
De fato, esta é uma abordagem comum na definição de tipos algébricos em Haskell, denominada *punning*.

```hs
> data Carta = Carta Naipe Valor                         deriving (Eq,Show,Ord)
Prelude> a = Carta Copas Ás
Prelude> :i a
a :: Carta 	-- Defined at <interactive>:39:1
Prelude> :t a
a :: Carta
Prelude> :i Carta
type Carta :: *
data Carta = Carta Naipe Valor
  	-- Defined at <interactive>:38:1
instance [safe] Ord Carta -- Defined at <interactive>:38:74
instance [safe] Show Carta -- Defined at <interactive>:38:69
instance [safe] Eq Carta -- Defined at <interactive>:38:66
Prelude> :t Carta
Carta :: Naipe -> Valor -> Carta
```


## Casamento de Padrões
Para que estes tipos sejam úteis, precisamos usá-los em funções, que é direto e óbvio para os tipos mais simples, como visto anteriormente.

```hs
corDoNaipe :: Naipe -> String
corDoNaipe Copas = "Vermelho"
corDoNaipe Ouro = "Vermelho"
corDoNaipe Paus = "Preto"
corDoNaipe Espada = "Preto"
```

Já para tipos que usam construtores, os padrões devem incluir o construtor, como mostram as funções a seguir.

```hs
naipe :: CartaT -> Naipe
naipe (Carta n _) = n

valor :: CartaT -> Valor
valor (Carta _ v) = v

valorNumérico :: Valor -> Int
valorNumérico Ás = 1
valorNumérico (Número i) = i
valorNumérico Valete = 11
valorNumérico Dama = 12
valorNumérico Rei = 13

> valorNumérico (Número 4)
4
> valorNumérico Rei
13
```

!!!exercise "Exercícios"
    * Usando tipos algébricos, defina os seguintes tipos e funções relacionados a jogos de cartas
        * Naipe
        * Valor
        * Carta
        * Jogo - lista de cartas (apelido, não tipo algébrico)
        * `éCanastra l` - função que `True` se `l` é uma sequência (possivelmente desordenada) de 7 cartas.
        * `temCanastra l` - função que `True` se `l` contem uma sub-lista que `éCanastra`

    * Usando tipos algébricos, defina as seguintes funções e tipos
        * Temperatura - tipo que pode conter um Tipo (Celsius, Kelvin, Farenheit) e um valor real.
        * tempInCelsius - função que recebe uma temperatura qualquer e retorna uma Temperatura em Celsius.
        * Item para outras temperaturas.

## Notação tipo "record"
???todo "TODO"
   Records


## Maybe

`#!hs Read` é uma classe de tipo útil por permitir que strings sejam usadas lidas e interpretadas como o tipo.
O tipo `#!hs Int` e outros números, por exemplo, pertencem a esta classe, o que nos diz que podemos fazer o seguinte:

```hs
> x = read "100" :: Int
> x
100
> x = read "100" :: Float
> x
100.0
```

Acontece que nem sempre a função será bem sucedida em interpretar a string, por exemplo:

```hs
> x = read "Bolhufas" :: Int
> x
*** Exception: Prelude.read: no parse
```

Esta é apenas uma de muitas situações em que uma exceção pode ser causada por uma falha na execução de alguma função.
Outros exemplos são falhas de alocação de memória, de abertura de um arquivo no disco, de comunicação com outro processo via uma rede de computadores, etc.
Nestas situações, é comum o uso do tipo `#!hs Maybe a` definido como se segue:

```hs
> import Text.Read

> :i Maybe
data Maybe a = Nothing | Just a
...
```

Este tipo permite que a função indique um erro ao retornar `#!hs Nothing` ou que um valor x foi recuperado da string usando ao retornar `#!hs Just x`.

```hs
> x = readMaybe "Bolhufas" :: Maybe Int
> x
Nothing
> x = readMaybe "100" :: Maybe Int
> x
Just 100
```



## Tipos recursivos
Como pode ver até agora, tipos algébricos tem muitos usos, a agora veremos um dos mais interessantes, na definição de tipos recursivos.
Considere uma lista, como definida pelo operador cons, `#!hs :`: uma lista é a concatenação de um elemento, a cabeça da lista, com uma outra lista, a cauda.
Usando tipos algébricos, conseguimos representar listas da seguinte forma:

```hs
data Lista a = Vazio | Elemento a (Lista a) deriving (Show)

busca :: a -> Lista a -> Bool
busca _ Vazio = False
busca e (Elemento x xs)
  | e == x = True
  | busca e xs

busca' :: a -> Lista a -> Bool
busca' _ Vazio = False
busca' e (Elemento x xs) = e == x || busca e xs


> lV = Vazio
> l1 = Elemento 1 (Vazio)
> l2 = Elemento 2 (Elemento 1 Vazio)
> lV
Vazio
> l1
Elemento 1 Vazio
> l2
Elemento 2 (Elemento 1 Vazio)

> busca' 1 lV
False
> busca' 1 l1
True
> busca' 1 l2
True
> busca' 2 l1
False
``` 

Para outro exemplo, considere uma árvore binária, uma estrutura de dados formada por nós que armazenam algum dado e apontam para outros dois nós, denominados filhos à esquerda e à direita.

```hs
---8<---
docs/code/tree.hs
---8<---
```

!!!exercise "Exercício"
    Implemente uma impressão "em ordem" dos nós da árvore.
