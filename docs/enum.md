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
> 
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

Se testá-lo, verá que não funciona. Um efeito semelhante é observado quando fazemos lgo mais simples ainda.

```hs
Prelude> data Naipe = Copas | Espada | Ouro | Paus
Prelude> Copas == Copas

<interactive>:2:1: error:
    • No instance for (Eq Naipe) arising from a use of ‘==’
    • In the expression: Copas == Copas
      In an equation for ‘it’: it = Copas == Copas
```

O problema é que Haskell não sabe como testar se dois naipes são iguais!
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


###### Ord






## Tipos mais complexos

Definir valores

```hs
data Rank = Ás | Número1 | Número2 | ... | Número10 | Valete | Dama | Rei
```

Definicao automática

```hs
data Rank = Ás | Número Int | Valete | Dama | Rei
```


###### Ord e Eq












Vamos definir outra enumeração, que represente a cor do naipe.

```hs
data Cor = Vermelho | Preto
    deriving (Show)

cor naipe = case naipe of 
        Copas -> Vermelho
        Ouro -> Vermelho
        Paus -> Preto
        Espada -> Preto
```