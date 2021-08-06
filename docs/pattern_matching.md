# Casamento de Padrões

Quando especificamos a lista de parâmetros formais que uma função recebe, estamos dizendo ao compilador que os parâmetros passados na invocação da função devem ser associados aos parâmetros formais.
Por exemplo, considere a seguinte definição de uma função que soma dois pontos 

```hs
soma2n x y = x + y
```

e sua invocação.

```hs
> soma 3 4
7
```

Quando a invocação acontece, o valor 3 é associado a `x` e 4 a `y`.
O mesmo acontece para qualquer tipo de parâmetro passado, mesmo tuplas e, como veremos depois, listas. 
Por exemplo, considere a soma de dois pontos em um espaço bidimensional, em que pontos são especificados como tuplas de aridade 2.

```hs
type Ponto = (Integer,Integer)

soma2v :: Ponto -> Ponto -> Ponto
soma2v ponto1 ponto2 = (fst ponto1 + fst ponto2, snd ponto1 + snd ponto2)
```

Um dos problemas deste código é a necessidade de usar `fst` e `snd` para extrair os componentes das tuplas.
Mas como vimos na seção sobre [tuplas](./tuples.md), é possível associar as componentes das tuplas diretamente a variáveis.
Por exemplo, considere a definição alternativa para `soma2v`.

```hs
type Ponto = (Integer,Integer)

soma2v :: Ponto -> Ponto -> Ponto
soma2v (x1,y1) (x2,y2) = (x1+x2, y1+y2)
```

Observe que, na linha 4, as coordenadas dos pontos passados são associados às variáveis `x1, y1, x2` e `y2`.
O termo correto para o que está acontecendo ali é **casamento de padrões**.

## Tipos de Padrão
O casamento de padrões é o processo pelo qual os valores em uma **expressão** são decompostos e associados aos elementos de um **padrão**.
Em outras palavras, dada uma expressão e um padrão, é feita uma tentativa de decomposição da expressão de acordo com o especificado pelo padrão.
Se a decomposição é bem sucedida, isto é, se **a expressão e o padrão tem exatamente o mesmo tipo** e se **constantes** especificadas no padrão tem o mesmo valor na mesma posição da expressão, então o casamento **é bem sucedido**; caso contrário, o casamento **fracassa**.

É mais fácil entender a descrição de casamento de padrões acima usando exemplos. 
Para isso, vamos dividir os padrões em alguns tipos e analisar exemplos de cada um destes tipos.

###### Constante
Considere a seguinte tabela, com suas colunas **padrão**, cujas entradas são **constantes**, **valor**, cujas cujas entradas queremos tentar casar com o padrão, e **resultado**, que informa o resultado do casamento.

        
| Padrão | Valor | Resultado |
|--------|-------|-----------|
| 10     | 10    | Sucesso   |
| 10     | 20    | Fracasso  |
| 10     | 'C'   | Erro      |
| True   | False | Fracasso  |
| False  | False | Sucesso   |


Para cada linha, imagine uma função definida como a seguir, mas onde o símbolo █ é substituído pelo padrão

```hs
minhaFunção █ = "Sucesso!"
```
e que você esteja invocando a função no ghci assim como a seguir, mas onde o símbolo ▓ é substituído pelo valor.

```hs
Prelude> minhaFunção ▓
```

Por exemplo, para a primeira linha, a função fica assim

```hs
minhaFunção 10 = "Sucesso!"
```

e a invocação fica assim.

```hs
Prelude> minhaFunção 10
```

Quando a invocação é feita, o ghci pega o valor passado, 10, e tenta casá-lo com o padrão especificado na definição de minhaFunção, 10.
Neste caso, há um casamento entre o valor e o padrão.

```hs
*Main> minhaFuncao 10
"Deu certo"
```

Já para a segunda linha, não há um casamento, pois o valor 10 não pode ser decomposto como a constante 20, e o ghci reclama com um erro que não vem ao caso agora.

```hs
*Main> minhaFuncao 20
"*** Exception: scratch.hs:87:1-28: Non-exhaustive patterns in function minhaFuncao
```

###### Variável

Se em vez de constantes a coluna padrão tivesse como elementos uma variável, então o casamento sempre seria bem sucedido.
Neste caso, a coluna *associações*[^bind] mostra quais os valores associados à cada variável do padrão. 

[^bind]: O termo em inglês é *bind*.

| Padrão | Valor | Resultado | Associação |
|--------|-------|-----------|------------|
| x      | 10    | Sucesso   | x = 10 |
| x      | 20    | Sucesso   | x = 20  |
| x      | 'C'   | Sucesso   | x = 'C' |
| x      | False | Sucesso   | x = False  |
| x      | (1,2,3) | Sucesso | x = (1,2,3) |

###### Curinga
Como já mencionado antes, `_` na definição de uma função funciona como uma variável, mas cujo valor é descartado, isto é, não é associado a uma variável.
Isto é na verdade um casamento em que o padrão é um **curinga**, que é sempre bem sucedido mas que não gera uma associação.

| Padrão | Valor | Resultado |
|--------|-------|-----------|
| _     | 10    | Sucesso |
| _     | 20    | Sucesso |
| _     | 'C'   | Sucesso |
| _     | False | Sucesso |
| _     | (1,2,3) | Sucesso |

###### Tupla
Como demonstrado no exemplo da função `#!hs soma2v`, tuplas podem ser usados como padrão, permitindo decompor uma tupla usada como valor.
O casamento de tuplas no valor e no padrão pode levar aos seguintes resultados, observando que o casamento de padrões é aplicado recursivamente em cada elemento da tupla.

* Sucesso se
    * a tupla padrão tiver a mesma aridade que a tupla valores **e**
    * cada elemento da tupla padrão casa com o elemento correspondente da tupla valor.
* Fracasso se
    * a tupla padrão tiver a mesma aridade que a tupla valore **e**
    * algum elemento da tupla padrão **não** casa com o elemento correspondente da tupla valor.
* Resulta em um erro de tipo se 
    * a tupla de padrões **não** tiver a mesma aridade que a tupla de valores **ou**
    * algum elemento da tupla de padrões resultar um erro de tipo no casamento com o elemento correspondente da tupla de valores.

| Padrão | Valor | Resultado | Associação |
|--------|-------|-----------|-------|
| (x,y) | (1,2) | Sucesso | x = 1 e y = 2 |
| (1,y) | (1,2) | Sucesso | y = 2 |
| (_,y) | (1,2) | Sucesso | y = 2 |
| ('X',y) | ('X',2) | Sucesso | y = 2 |
| (x,y) | (1,(2,3)) | Sucesso | x = 1; y = (2,3) |
| (\_, (\_,y)) | (1,(2,3)) | Sucesso | y = 3 |
| (x,y) | (1,(2,3),3) | Erro de tipo| |
| (1,y) | ('x',(2,3)) | Erro de tipo| |
| x | (1,2) | Sucesso | x = (1,2) |
| (x,y) | 1 | Erro de tipo| |


###### Listas
Por completude, precisamos mencionar que o casamento funciona também para listas, mas deixaremos para mais tarde esta discussão, quando nos focarmos em listas.

###### Mais de um padrão
Nos exemplos vistos nas tabelas, temos sempre um valor e um padrão, mas funções podem ter diversos parâmetros.
Neste caso, o casamento de padrões acontece para cada um dos parâmetros passados na invocação da função, da esquerda para a direita.
Por exemplo, considerando a seguinte definição

```hs
minhaFunção'' (a,b) ((c,d),_,f)  g = a + b + c + d + f + fst g + snd g
```

invocação 

```hs
*Main> minhaFunção''  (1,2) ((3,4),5,6) (7,8)
31
```

Isto é, temos as seguintes associações `a = 1, b = 2, c = 3, d = 4, f = 6, g = (7,8)`.


## Definição de Funções
Além de decompor os parâmetros passados para um função e como mencionado [anteriormente](../guards), casamento de padrões pode ser usado na definição de funções para simplificar testes via `if-then-else` e guardas nos parâmetros da função.

Neste caso, a função é definida como uma sequência de equações em que são feitas tentativas sucessivas de casamento de padrões, na ordem das definições.
O resultado da invocação da função é dado pela **primeira** equação em que houver um casamento bem sucedido **e** todas as guardas forem satisfeitas.
Se ao final não houver casamento ou se as guardas não forem satisfeitas, ocorre um **erro de execução**.

Para começar, vejamos uma definição sem guardas, revisitando a função `nomeMes`.
Usando casamento de padrões constantes, ela ficaria assim:

```hs
nomeMes 1 = "JAN"
nomeMes 2 = "FEB"
nomeMes 3 = "ABR"
nomeMes 4 = "MAR"
nomeMes 5 = "MAI"
nomeMes 6 = "JUN"
nomeMes 7 = "JUL"
nomeMes 8 = "AGO"
nomeMes 9 = "SET"
nomeMes 10 = "OUT"
nomeMes 11 = "NOV"
nomeMes 12 = "DEZ"
```

Esta função funciona especificamente para valores na faixa [1,12] e retornará um erro para qualquer valor fora da mesma.

```hs
*Main> nomeMes 13
"*** Exception: scratch.hs:(112,1)-(123,18): Non-exhaustive patterns in function nomeMes
```

É possível usar uma definição genérica *catch-all* para casar com valores não específicos usando um padrão variável.
Por exemplo, a definição da função `fatorial` tem um tratamento especial para 0, via padrão constante, e um caso genérico para qualquer outro número, via padrão variável.

```hs
fatorial 0 = 1
fatorial n = n * fatorial (n-1)
```

Mas esta definição de `fatorial` tem um problema, que aparece ao se tentar calcular o fatorial de números negativos, que são indefinidos.
Neste caso, precisamos impedir que números negativos sejam aceitos pela função, lançando erros.

```hs
fatorial 0 = 1
fatorial n 
    |n > 0 = n * fatorial (n-1)
    |otherwise = error "Indefinido"

fatorial' 0 = 1
fatorial' n
    |n > 0 = n * fat (n-1)
fatorial' _ = error "Indefinido"
```

Com o seguinte resultado 

```hs
*Main> fat (-1)
*** Exception: Indefinido
```


O exemplo seguinte mostra o uso de padrões curinga, constantes e variáveis, combinados com guardas e, um caso especial, gerando um erro.
Você consegue determinar o que a função faz?

```hs
próximos3 :: Int -> Char -> (Int,Int,Int)
próximos3 0 _ = (-1,0,1)
próximos3 n 'd'
    | n > 0 = (n-1,n-2,n-3)
    | n < 0 = (n+1,n+2,n+3)

próximos3 n 'a'
    | n < 0 = (n-1,n-2,n-3)
    | n > 0 = (n+1,n+2,n+3)

próximos3 n _ = error "Use d ou a"
```


Vejamos outros exemplos, do módulo prelude do Haskell.
Primeiro, a função `not`, que nega o valor passado.

```hs
not :: Bool -> Bool
not True = False
not False = True
```

Vejamos agora diferentes definições do operador `#!hs &&`. Observe que como o operador é infixo, a definição das equações segue esta notação, mesmo que a definição do protótipo não siga.

A primeira definição usa somente constantes como padrão.
Esta definição está correta, mas é mais complexa do que o necessário, já que somente a primeira equação resulta em `#!hs True`.

```hs
(&&) Bool -> Bool -> Bool
True && True = True
True && False = False
False && True = False
False && False = False
```

Uma versão simplificada pelo uso de padrões curinga, seria a seguinte.
Observe como ela é mais legível.

```hs
(&&) Bool -> Bool -> Bool
True && True = True
_ && _ = False
```

Finalmente, uma terceira versão que usa um padrão variável e um curinga também poderia ser usada.

```hs
(&&) Bool -> Bool -> Bool
True && b = b
False && _ = False
```


Como exemplo do casamento em padrões em tuplas, relembre as definições de `#!hs fst` e `#!hs snd`.

```hs
fst (x,_) =  x

snd (_,y) =  y
```

Também podemos pensar no tipo `#!hs Pessoa` e funções associadas, definidos anteriormente.
Neste caso, como ficaria uma função que extraísse apenas o sobrenome de uma pessoa?

```hs
--8<--
docs/code/pessoa3.hs
--8<--
```


!!!exercise "Exercício"
    * Seguindo os moldes da definição do operador `&&`, defina o operador lógico ou `||` de três formas diferentes.
    * Seguindo os moldes da definição do operador `&&`, defina o operador lógico ou `||` de três formas diferentes.
    * Defina 3 funções, usando `if`-`then`-`else`, guardas e casamento de padrões, que calculem os números da série de Fibonacci, a saber
        * Fib(1) = 1
        * Fib(2) = 1
        * Fib(n) = Fib(n-1) + Fib(n-2)

        ???example "Resolução"
            ```hs
            Surprise!
            ```
        













###### `#!hs case-of`


Esta estrutura se assemelha ao `switch` de linguagens como C e Java, e tem a seguinte sintaxe:

```hs
case expression of pattern -> result  
                   pattern -> result  
                   pattern -> result  
```

Por exemplo, vamos definir uma função que retorne o nome do mês, dado o seu número.
Seria possível escreve esta função com `if` aninhados, assim.

```hs
nome_mes m = if m == 1 then "JAN"
             else if m == 2 then "FEB"
             else if m == 3 then "MAR"
             ...
             else if m == 11 then "NOV"
             else "DEZ"
```

Usando guardas, ficaria assim:

```hs
nome_mes m
    | m == 1 = "JAN"
    | m == 2 = "FEB"
    | m == 3 = "MAR"
    ...
    | m == 11 = "NOV"
    | otherwise "DEZ"
```

Usando `case`-`of`, a

```hs
nome_mes m = case m of 1 -> "JAN"
                  m of 2 ->  "FEB"
                  m of 3 -> "MAR"
                  ...
                  m of 11 -> "NOV"
                  m of 12 -> "DEZ"
```