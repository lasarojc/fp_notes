# Tipos

Seja a função `dias_mes` a função que calcula a quantidade de dias em um mês, dado o número do mês, definida assim:

```hs
dias_mes m
    | m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12 = 31
    | m == 2 = 28
    | otherwise = 30
```

O que acontece se você passar 7.2 para a função? O resultado está correto? 
Isso nos leva a perguntar como definir que uma função só é aplicável a números inteiros, do **tipo** inteiro.

Em linguagens de programação, um tipo é um nome dado a uma coleção de valores que tem um mesmo comportamento na linguagem.
Na linguagem C, por exemplo, temos os tipos `int` e `short`, ambos associados a valores inteiros mas com número de bits diferentes usados para representá-los.

Algumas linguagens são denominadas fracamente tipadas, o que quer dizer que o uso de tipos é relaxado e pode até ser alterado em tempo de execução.
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

???sideslide "Tipagem""
    * Forte
    * Estática
    * Inferência

Já a linguagem Haskell é o que chamamos de **fortemente tipada**, o que quer dizer que toda variável, constante ou função tem apenas um tipo e este sempre pode ser determinado.
Além disso, Haskell é **estaticamente tipada**, ou seja, os tipos são determinados em tempo de compilação, em oposição a linguagens que determinam o tipo durante a execução do programa, denominadas dinamicamente tipadas.
Assim como `type` em Python, Haskell tem a função `#!hs :type` para verificar o tipo de uma variável. Por exemplo:

```hs
   :type 1
1 :: Num p => p
   :type 1.0
1.0 :: Fractional p => p
   :type "Bom dia"
"Bom dia" :: [Char]
```

Finalmente, se olharmos novamente para os exemplos de funções anteriores, veremos que não foram definidos tipos em suas declarações; isto é possível porquê Haskell consegue **inferir** os tipos dos dados de forma muito eficiente, obrigando você a explicitá-los apenas em casos muito específicos.

Haskell tem um número imenso de tipos pre-definidos, organizados hierarquicamente, e que podem ser usados para definir novos tipos pelo programador.
Mas por enquanto, nos foquemos nos tipos mais simples.

## Tipos Primitivos e Operadores

###### Tipos numéricos
Como toda linguagem de programação, Haskell tem vários tipos pré-definidos para representar números, como mostra a seguinte tabela.

| Tipo | Descrição| Valores|
|------|------|----|
| `Int` | Inteiro de precisão finita| |
| `Integer` | Inteiro de precisão arbitrária| |
| `Float`  | Ponto flutuante de precisão simples | |
| `Double`  | Ponto flutuante de precisão dupla | |

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


###### Caracteres
Para a representação de caracteres individuais, Haskell usa o tipo `Char`, e para sequências de caracteres, usa o tipo `String`, que é um "apelido" para **lista** de `Char`.
Como lista não é um tipo primitivo, ou melhor, mesmo que em Haskell listas sejam fundamentais, não as estudaremos agora.
Ainda assim, usaremos `String` em alguns exemplos, mas sem entrar em detalhes da manipulação de listas.
Exemplos dos dois tipos são apresentados na tabela a seguir.

| Tipo | Descrição| Valores|
|------|------|----|
|`Char` | Caractere | '1'; 'a'; 'B'|
|`String`| Sequência de caracteres | "Eu"; "Hello"; "Zabumba" |

Algumas funções úteis na manipulação de caracteres e strings são apresentadas na tabela.

???todo "Funções"
    tabela com "operadores"


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




!!!todo "Exercícios"
    * Usar todos os operadores
    * Evidenciar diferenças entrem quot e div.


## Protótipo de funções
Agora que já conhecemos alguns tipos, podemos ver como usá-los na definição de funções.
Para fazê-lo, devemos usar a seguinte sintaxe.

```hs
nome_funcao :: tipo_arg1 -> ... -> tipo_argN -> tipo_saida
nome_funcao arg1 ... argN = <definicao>
```

Por exemplo, o **protótipo** da função `dia_mes`, isto é, a definição dos tipos de entrada e saída da função, fica assim:

```hs
dias_mes :: Int -> Int
dias_mes m
    | m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12 = 31
    | m == 2 = 28
    | otherwise = 30
```


!!!exercise "Exercícios"
    * Defina o protótipo da função de conversão de Fahrenheit para Celsius `#!hs f2c x = (x - 32) /1.8`
    * Descubra a quanto 100f corresponde em Célsius

    ???example "Resolução"
        ```
        hs f2c :: Float -> Float`
        f2c x = (x - 32) /1.8
        ```

    * Defina o protótipo da função de media de dois números reais `#!hs m2n a b = (a + b)/2`
    
    ???example "Resolução"
        `#!m2n :: Float -> Float -> Float`


    * Defina o protótipo da soma de dois números inteiros `#!hs soma2int a b = a + b`
    * Aplique a função aos valores 2 e 3.
    * Aplique a função aos valores 2.0 e 3.0.
    
    ???example "Resolução"
        `#!soma2int :: Int -> Int -> Int`

    * Defina o protótipo da soma de dois números reais `#!hs soma2reais a b = a + b`
    * Aplique a função aos valores 2.0 e 3.0.
    * Aplique a função aos valores 2 e 3.
    
    ???example "Resolução"
        `#!soma2reais :: Float -> Float -> Float`

    * Explique a diferença de comportamento das duas últimas funções.

    ???example "Resolução"
        As funções se comportam diferentemente, sendo que a primeira mostra um erro quando aplicada a dois números reais, porquê $Z \subset  R$ mas $R \not\subset Z$.
