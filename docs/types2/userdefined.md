

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



## Tuplas como tipos

Como visto anteriormente, a palavra reservada `#!hs type` permite que definamos **apelidos** para tipos no Haskell.
Pois tuplas também podem ser associadas a tipos, por exemplo, podemos definir que **Pessoa** é o tipo definido na seção anterior, i.e., uma tupla dos campos nome, telefone, CPF e endereço.

```hs
--8<--
docs/code/pessoa2.hs
--8<--
```

Se perguntarmos ao Haskell qual o tipo da tupla gerada pela função `#!hs fazPessoa`, ele responderá `#!hs Pessoa`.

```hs
> :t fazPessoa
fazPessoa :: String -> String -> String -> String -> Pessoa
> p = fazPessoa "Jose" "Tel" "CPF" "End"
> :t p
p :: Pessoa
```

Podemos ir além e definir tipos usando outros tipos estruturados. Por exemplo:

```hs
--8<--
docs/code/pessoa3.hs
--8<--
```

Neste caso

```hs
> p = fazPessoa ("José","da","Silva") ("ddd","numero")  "CPF"  ("Rua da Couves","143","Brasil")
> p
(("Jos\233","da","Silva"),("ddd","numero"),"CPF",("Rua da Couves","143","Brasil"))
> :t p
p :: Pessoa
> n = pegaNome p
> n
("Jos\233","da","Silva")
> :t n
n :: Nome
```

Vejamos outro exemplo; sejam datas, tuplas de 3 inteiros: dia, mês e ano. Assim, 25 de dezembro de 1999 é `#!hs (25, 12, 1999)`.
Dado duas datas válidas, uma operação interessante é testar se uma data é menor que outra.

```hs
--8<--
docs/code/dates1.hs
--8<--
```

Mas esta função pode ser descartada com a escolha da definição de data, pois tuplas são naturalmente ordenáveis.

!!!exercise "Data Válida"
    * Defina um tipo para representar datas como tuplas.
    * Defina uma função `#!hs dataValida` que receba uma data e retorne `True` se a data for válida e `False` se for inválida.
    * Por exemplo, 38 de **onzembro** de 2021 é uma data inválida, assim como 29 de fevereiro de 2017, mas dia primeiro de Janeiro de 2000 é válida. Isto é,
        * `#!hs dataValida (38, 13, 2021) == False`
        * `#!hs dataValida (29, 02, 2017) == False`
        * `#!hs dataValida (1,1,2000) == True`

    ???example "Resolução"
        ```hs
        --8<--
        docs/code/dates2.hs
        --8<--
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