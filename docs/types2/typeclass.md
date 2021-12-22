
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