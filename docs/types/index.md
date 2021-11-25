# Tipos

Seja `#!hs diasMes` a função que calcula a quantidade de dias em um mês, dado o número do mês, definida assim:

```hs
diasMes m
    | m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12 = 31
    | m == 2 = 28
    | otherwise = 30
```

O que acontece se você passar 7.2 como argumento para a função? O resultado está correto? 
Isso nos leva a perguntar como definir que uma função só é aplicável a números inteiros, do **tipo** inteiro.

Em linguagens de programação, um tipo é um nome dado a uma coleção de valores que tem um mesmo comportamento na linguagem.
Em C, por exemplo, temos os tipos `#!c int` e `#!c short`, ambos associados a valores inteiros mas com número de bits diferentes usados para representá-los.

Algumas linguagens são denominadas **fracamente tipadas**, o que quer dizer que o uso de tipos é relaxado e pode até ser alterado em tempo de execução.
Por exemplo, no seguinte código em Python a variável `x` assume três tipos diferentes, em momentos diferentes.

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

Já a linguagem Haskell é o que chamamos de **fortemente tipada**, o que quer dizer que toda variável, constante, e função tem apenas um tipo e este sempre pode ser determinado.
Além disso, Haskell é **estaticamente tipada**, ou seja, os tipos são determinados em tempo de compilação, em oposição às linguagens dinamicamente tipadas, que determinam o tipo durante a execução do programa.

Para definir o tipo de uma expressão, usa-se após a expressão `#!hs :: <Tipo>`, como nos seguintes exemplos.

```hs
> x = 1::Int
> y = 1::Integer
> z = 1::Double
```

Assim como `type` em Python, o GHCi tem o comando `#!hs :type` (ou simplesmente `#!hs :t`) que permite verificar o tipo de uma expressão. Por exemplo:

```hs
> :type x
x :: Int
> :t y
y :: Integer
> :t z
z :: Double
```

Acontece que se se olharmos novamente para os exemplos de código em Haskell vistos no capítulo anterior, veremos que em lugar algum foram definidos tipos; isto é possível porquê Haskell consegue **inferir** os tipos dos dados de forma muito acurada, olhando para as funções que manipulam os dados. Por exemplo, na pela definição da função `#!hs quad x = x*x`, Haskell sabe que `#!hs x` precisa ser operável com `*`, logo, precisa ser um número.

Apesar da capacidade de inferência do Haskell, frequentemente especificamos tipos, principalmente para funções, mais como uma forma de facilitar a leitura e manutenção do código e para indicar sua intenção ao compilador, que irá testar se os tipos indicados podem ser satisfeitos pelo código e lhe informar caso contrário.