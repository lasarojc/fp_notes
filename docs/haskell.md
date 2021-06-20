# Breve introdução a Haskell

A linguagem de programação Haskell, cujo nome é uma homenagem ao matemático e lógico **Haskell B. Curry**, é famosa por ser uma linguagem funcional **pura**, de **propósito geral** e características marcantes como **avaliação preguiçosa**, e tipagem **estática**, **forte** e por **inferência**.

A primeira versão da linguagem Haskell apareceu em 1987 do esforço para se consolidar vários avanços propostos no paradigma funcional.
Mais do que uma linguagem, Haskell era uma especificação, ou série de especificações, tendo tido várias implementações distintas.

Haskell 98 foi um marco da linguagem, sendo uma versão considerada estável.
Nesta época surgiu o Glasgow Haskell Compiler (GHC), que se tornou o compilador Haskell "padrão".

A versão seguinte da linguagem começou a ser especificada em 2006 e anunciada em 2009, a Haskell 2010. Dentre os principais avanços desta versão está a possibilidade de interagir com código escrito em outras linguagens, via the *foreign function interface* (FFI).

Mas chega de história. Vejamos alguns exemplos simples da linguagem.

## Exemplos simples
Para começar, vejamos um trecho de código com uma iteração simples, calculando o somatório de 1 a 10, em linguagem C.

```c
int total = 0;
for (int i = 1; i < 11; i++)
    total = total + i;
```

Em Haskell, temos diversas opções para obter o mesmo resultado, por exemplo.

```Haskell
iteracao

total = sum [1...10]

fold
```


## Tipos básicos

## Instalando o GHC












Fortemente tipada: sem conversão automática
Statically typed: em tempo de compilação.

exemplo python e tipos mutantes


inferência de tipos.
tipos explícitos

tipos especificados pelo usuário
tipos primitivos


Int
Integer
Double
Float
Bool
Char
String é lista de caracteres
