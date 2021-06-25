# Tipos
Em linguagens de programação, um tipo é um nome dado a uma coleção de valores que são tem um mesmo comportamento na linguagem.
Na linguagem C, por exemplo, temos os tipos `int` e `short`, ambos associados a valores inteiros mas número de bits diferentes usados para representá-los.

Algumas linguagens são denominadas fracamente tipadas, o que quer dizer que o uso de tipos é relaxado e pode até ser alterado em tempo de execução.
Por exemplo, no seguinte código em Python a variável `x` assume três tipos diferentes.

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
Finalmente, se olharmos novamente para os exemplos de funções anteriores, veremos que não foram definidos tipos em suas declarações; isto é possível porquê Haskell consegue **inferir** os tipos dos dados de forma muito eficiente, obrigando você a explicitá-los apenas em casos muito específicos.

Haskell tem um número imenso de tipos pre-definidos, organizados hierarquicamente, e que podem ser usados para definir novos tipos pelo programador.
Mas por enquanto, nos foquemos nos tipos mais simples.

## Tipos Primitivos e Operadores
Como toda linguagem de programação, Haskell tem tipos pré-definidos para representar números e caracteres, como mostra a seguinte tabela.

| Tipo | Descrição| Valores|
|------|------|----|
| `Int` | Inteiro de precisão finita| |
| `Integer` | Inteiro de precisão arbitrária| |
| `Float`  | Ponto flutuante de precisão simples | |
| `Double`  | Ponto flutuante de precisão dupla | |
| `Char`  | Caractere | |


Além do tipo `Char`, Haskell define também o tipo `String` como uma **lista** de `Char`.
Como lista não é um tipo primitivo, ou melhor, mesmo que em Haskell listas sejam um tipo básicos, não as estudaremos agora. 
Ainda assim, usaremos `String` em alguns exemplos, de forma bem descomplicada.



Dados os tipos primários, o próximo passo é ver os operadores que os manipulam.

|Operador|Operação|
|----|----|
| `+` | Adição|
| `-` | Subtração |
| `*` | Multiplicação| 
| `/` | Divisão|
| `^` | Exponenciação, e.g, `2^4 == 16`|
| `-` | Inversão de sinal, e.g, `- (-10) == 10`[^negate]|
| `negate` | Inversão de sinal, e.g, `negate (-10) == 10`[^negate]|
| `quot`| Divisão inteira, truncado pro 0|
| `rem`| Resto da divisão inteira, tal que ```(x `quot` y)*y + (x `rem` y) == x```|
| `div`| Resto da divisão, truncado para baixo|
| `mod`| Módulo do inteiro, tal que  ```(x `div` y)*y + (x `mod` y) == x```|



Observe que alguns destes operadores são naturalmente infixos, e.g., `+`, e outros prefixos, e.g., `rem`.
Contudo, ambos podem ser usados da outras forma, como mostrado nos seguintes exemplos.

[^negate]: Observe que o o `-10` está entre parênteses não porque o operador demanda, mas para deixar claro que o `-` faz parte do número.

```hs
10 + 20
(+) 10 20
quot 10 3
10 `quot` 3
```



| Tipo | Descrição| Valores|
|------|------|----|
|`Bool` | Booleano | `True` ou `False`|


|Operador|Operação|
|----|----|
| `&&` | E lógico|
| `||` | OU lógico|
| `not`| Negação|
| `==`| Igualdade| 
| `/=`| Diferença|




!!!todo "Exercícios"
    * Usar todos os operadores
    * Evidenciar diferenças entrem quot e div.


## A função `:type`


## Protótipo de funções

```hs
nome_funcao :: tipo_arg1 -> ... -> tipo_argN -> tipo_saida
nome_funcao arg1 ... argN = <definicao>
```

!!!exercise "Exercício"
    * Defina o protótipo da função de conversão de Fahrenheit para Celsius `#!hs f2c x = (x - 32) /1.8`

    ???example "Resolução"
        `#!hs f2c :: Float -> Float`

    * Defina o protótipo da função de media de dois números reais `#!hs m2n a b = (a + b)/2`
    
    ???example "Resolução"
        `#!media :: Float -> Float -> Float`

