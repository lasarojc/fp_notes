# Apelidos

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


Além de apelidos para tipos simples, `#!hs type` permite que definamos tipos mais complexos.

## Tuplas como tipos
Usando `#!hs type` podemos definir que **Pessoa** é o tipo definido na seção anterior, i.e., uma tupla dos campos nome, telefone, CPF e endereço.

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