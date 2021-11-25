# Tipos Básicos e seus Operadores
Como toda linguagem de programação, Haskell tem vários tipos pré-definidos para representar números, valores booleanos, e (sequências de) caracteres.
Algo a se notar é que os **nomes dos tipos são iniciados com letras maiúsculas**, diferentemente dos nomes de funções e variáveis.

## Números
Os tipos básicos para a representação de números em Haskell são apresentados na seguinte tabela.
Observe que o tipo `#!hs Integer` ter precisão arbitrária, ou seja, pode ser usado para representar números com "qualquer" quantidade de dígitos, diferentemente de `#!hs Int`, que só consegue representar números em uma faixa específica (64 bits).

###### Tipos numéricos
| Tipo |        Descrição       | Valores|
|------|------|----|
| `#!hs Int` | Inteiro de precisão finita| 4, -14, 2147483647|
| `#!hs Integer` | Inteiro de precisão arbitrária| 30414093201713378043612608166064768844377641568960512000000000000|
| `#!hs Float`  | Ponto flutuante de precisão simples | 25.132742 |
| `#!hs Double`  | Ponto flutuante de precisão dupla | 25.132741228718345 |

###### Operadores
Diversos operadores são definidos para os tipos numéricos básicos, como mostra a tabela a seguir.

|Operador|Operação| Exemplo |
|----|----|----|
| `#!hs +` | Adição| `#!hs 1 + 1 == 2`|
| `#!hs -` | Subtração | `#!hs 1 - 1 == 0`|
| `#!hs *` | Multiplicação| `#!hs 1 * 1 == 1`|
| `#!hs /` | Divisão| `#!hs 1 / 1 == 1`|
| `#!hs ^` | Exponenciação | `#!hs 2^4 == 16`|
| `#!hs -` | Inversão de sinal[^negate] | `#!hs - (-10) == 10`|
| `#!hs negate` | Inversão de sinal[^negate] | `#!hs negate (-10) == 10`|
| `#!hs quot`| Divisão inteira, truncado para o 0, i.e., `#!hs quot a b` é igual a $\lfloor\frac{a}{b}\rfloor$ se $a>0$ e $\lceil\frac{a}{b}\rceil$ se $a<0$ | `#!hs quot 8 3 == 2` e `#!hs quot (-8) 3 == -2` |
| `#!hs rem`| Resto da divisão inteira, tal que ```#!hs (x `quot` y)*y + (x `rem` y) == x```| `#!hs rem 8 3 == 2` e `#!hs rem (-8) 3 == -2`|
| `#!hs div`| Resto da divisão, truncado para baixo, i.e., `#!hs div a b` é igual a $\lfloor\frac{a}{b}\rfloor$ | `#!hs div 8 3 == 2` e `#!hs div (-8) 3 == -3`|
| `#!hs mod`| Módulo do inteiro, tal que  ```#!hs (x `div` y)*y + (x `mod` y) == x```|  `#!hs mod (-8) 3 == 1` e `#!hs  mod (8) 3 == 2` |


###### Infixo X Prefixo
Observe que alguns destes operadores são naturalmente infixos, e.g., `+`, e outros, prefixos, e.g., `#!hs rem`.[^infix]
Contudo, ambos podem ser usados da outra forma, como mostrado nos seguintes exemplos, em que os operadores infixos são envelopados em por parênteses e os pré-fixos em aspas invertidas simples.



[^infix]: Operadores são funções cujos nomes só contem caracteres especiais. Eles são naturalmente infixos.

[^negate]: Observe que o o `-10` está entre parênteses não porque o operador demanda, mas para deixar claro que o `-` faz parte do número.

```hs
10 + 20
(+) 10 20
quot 10 3
10 `quot` 3
```


###### Tipos para números
Caso você queira ou precise, pode especificar um tipo para um número, por exemplo, `#!hs 20 :: Int` ou  `#!hs 20 :: Double`.

!!!todo "Exercícios"
    * Usar todos os operadores
    * Evidenciar diferenças entrem quot/rem e div/mod.

## Booleanos
Para representação de valores lógicos, Haskell define o tipo `#!hs Bool`.

| Tipo | Descrição| Valores|
|------|------|----|
|`#!hs Bool` | Booleano | `#!hs True` ou `#!hs False`|

Para este tipo, temos os seguintes operadores.

|Operador|Operação|
|----|----|
| `#!hs &&` | E lógico|
| `#!hs ||` | OU lógico|
| `#!hs not`| Negação|
| `#!hs ==`| Igualdade| 
| `#!hs /=`| Diferença|

## Caracteres
Finalmente, para a representação de caracteres individuais, Haskell usa o tipo `#!hs Char`, e para sequências de caracteres, usa o tipo `#!hs String`.
Exemplos dos dois tipos são apresentados na tabela a seguir.

| Tipo | Descrição| Valores|
|------|------|----|
|`#!hs Char` | Caractere | '1'; 'a'; 'B'|
|`#!hs String`| Sequência de caracteres | "Eu"; "Hello"; "Zabumba" |

O tipo `#!hs String` é, na verdade, apenas um "apelido" para o tipo **lista de caracteres** e, assim sendo, voltaremos a falar de `#!hs String` quando estudarmos listas, mais adiante.

Haskell trabalha com caracteres Unicode, que incluem símbolos e letras em diferentes línguas, números, sinais de pontuação, separadores e outros caracteres especiais, como emojis e caracteres de controle.
A seguir, algumas funções de manipulação de caracteres.[^char]

[^char]: Diversas outras funções estão disponíveis no pacote [Data.Char](https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Char.html).

| Função | Descrição| Exemplo|
|------|------|----|
|`#!hs isSpace`| Testa se é um espaço em branco ou um dos seguintes caractere de controle `\t`, `\n`, `\r`, `\f` ou `\v` | `#!hs isSpace '\t' == True`  |
|`#!hs isControl` | Testa se é um caracter de controle | `#!hs isControl '\t' == True`  |
|`#!hs isLetter` | Testa se é uma letra | `#!hs isLetter 'A' == True` `#!hs isLetter '\t' == False` |
|`#!hs isDigit` | Testa se é um dígito | `#!hs isDigit 'A' == False` `#!hs isDigit '3' == True` |
|`#!hs isLower` | Testa se é uma letra minúscula | `#!hs isLower 'a' == True` `#!hs isLower '\t' == False` |
|`#!hs isUpper` | Testa se é uma letra maiúscula | `#!hs isUpper 'A' == True` `#!hs isUpper '\t' == False` |
|`#!hs toLower` | Transforma uma letra em minúscula | `#!hs toLower 'a' == 'a'` `#!hs toLower 'A' == 'a'` |
|`#!hs toUpper` | Transforma uma letra em  maiúscula | `#!hs toUpper 'A' == 'A'` `#!hs toUpper 'a' == 'A'` |

