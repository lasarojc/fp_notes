# Casamento de Padrões
Em seções anteriores mencionamos quando especificamos a lista de parâmetros formais que uma função recebe, estamos dizendo ao compilador como os parâmetros passados na invocação da função devem ser associados aos parâmetros formais por meio de um **casamento de padrões**.

Mais formalmente, o casamento de padrões é o processo pelo qual os valores em uma **expressão** são decompostos e associados aos elementos de um **padrão**.
Em outras palavras, dada uma expressão e um padrão, é feita uma tentativa de decomposição da expressão de acordo com o especificado pelo padrão.
Se a decomposição é bem sucedida, isto é, se **a expressão e o padrão tem exatamente o mesmo tipo** e se **constantes** especificadas no padrão tem o mesmo valor na mesma posição da expressão, então o casamento **é bem sucedido**; caso contrário, o casamento **fracassa**.

É mais fácil entender esta descrição se dividirmos os padrões em alguns tipos e analisar exemplos de cada um destes.

## Tipos de Padrão
Padrões podem ser definidos primariamente em **constante**, **variável**, **curinga**, **tupla** e **lista**, todos já usados em exemplos anteriormente, mas vistos aqui de forma mais detalhada.
Estes, por sua vez, podem ser usados na composição de padrões mais complexos.

###### Constante
Considere a seguinte tabela, com suas colunas **padrão**, cujas entradas são **constantes**, **valor**, cujas entradas queremos tentar casar com o padrão, e **resultado**, que informa o resultado do casamento.
Um casamento com um padrão constante será bem sucedido se a expressão tem exatamente o mesmo valor (e portanto o mesmo tipo) que o padrão.

        
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
> minhaFunção ▓
```

Por exemplo, para a primeira linha, a função fica assim

```hs
minhaFunção 10 = "Sucesso!"
```

e a invocação fica assim.

```hs
> minhaFunção 10
```

Quando a invocação é feita, o ghci pega o valor passado, 10, e tenta casá-lo com o padrão especificado na definição de minhaFunção, 10.
Neste caso, há um casamento entre o valor e o padrão.

```hs
*Main> minhaFuncao 10
"Deu certo"
```

Já para a segunda linha, não há um casamento, pois o valor 10 não pode ser casado como a constante 20 (e o ghci reclama que a função não foi definida de forma a casar com todos os valores possíveis, mas isso não vem ao caso agora).

```hs
*Main> minhaFuncao 20
"*** Exception: scratch.hs:87:1-28: Non-exhaustive patterns in function minhaFuncao
```

###### Variável

Se em vez de constantes a coluna padrão tivesse como elementos uma variável, então o casamento sempre seria bem sucedido.
Neste caso, a coluna *Associação*[^bind] mostra quais os valores associados à cada variável do padrão. 

[^bind]: O termo em inglês é *bind*.

| Padrão | Valor | Resultado | Associação |
|--------|-------|-----------|------------|
| x      | 10    | Sucesso   | x = 10 |
| x      | 20    | Sucesso   | x = 20  |
| x      | 'C'   | Sucesso   | x = 'C' |
| x      | False | Sucesso   | x = False  |
| x      | (1,2,3) | Sucesso | x = (1,2,3) |

###### Curinga
Como já mencionado antes, se o valor de um parâmetro não importa na definição de uma função, então este valor pode ser casado com `_`,  que funciona como uma variável, mas cujo valor é descartado.
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
    * a tupla padrão tiver a mesma aridade que a tupla valores **ou**
    * algum elemento da tupla padrão **não** casa com o elemento correspondente da tupla valor.
* Resulta em um erro de tipo se 
    * a tupla de padrões **não** tiver a mesma aridade que a tupla de valores **ou**
    * algum elemento da tupla de padrões resultar um erro de tipo no casamento com o elemento correspondente da tupla de valores.

| Padrão | Valor | Resultado | Associação |
|--------|-------|-----------|-------|
| (x,y) | (1,2) | Sucesso | x = 1 e y = 2 |
| (1,y) | (1,2) | Sucesso | y = 2 |
| (1,y) | (2,2) | Fracasso | |
| (_,y) | (1,2) | Sucesso | y = 2 |
| (_,y) | (10,2) | Sucesso | y = 2 |
| ('X',y) | ('X',2) | Sucesso | y = 2 |
| (x,y) | (1,(2,3)) | Sucesso | x = 1; y = (2,3) |
| (\_, (\_,y)) | (1,(2,3)) | Sucesso | y = 3 |
| (x,y) | (1,(2,3),3) | Erro de tipo| |
| (1,y) | ('x',(2,3)) | Erro de tipo| |
| x | (1,2) | Sucesso | x = (1,2) |
| (x,y) | 1 | Erro de tipo| |


###### Listas

No casamento com listas, a expressão é decomposta por um padrão do mesmo tipo, nos elementos especificados no padrão.

* Sucesso se
    * a lista padrão tiver o mesmo tipo que a lista valor **e**
    * cada elemento da explicitado na lista padrão casa com o elemento correspondente lista valor **e**
    * se uma cauda é especificada na lista padrão então a cauda casa com a lista valor.
* Fracasso se
    * a lista padrão tiver um tipo diferente da tupla valor **ou**
    * algum elemento da lista padrão **não** casa com o elemento correspondente da lista valor.
* Resulta em um erro de tipo se 
    * a lista de padrões **não** tiver o mesmo tipo que a lista de valores.

| Padrão | Valor | Resultado | Associação |
|--------|-------|-----------|-------|
| [x,y] | [1,2] | Sucesso | x = 1 e y = 2 |
| [1,y] | [1,2] | Sucesso | y = 2 |
| [1,y] | [2,2] | Fracasso | |
| [1,y] | [1] | Fracasso | |
| [_,y] | [1,2] | Sucesso | y = 2 |
| [_,y] | [10,2] | Sucesso | y = 2 |
| x:y | [1,2] | Sucesso | x = 1 e y = [2] |
| 1:y | [1,2] | Sucesso | y = [2] |
| 1:y | [2,2] | Fracasso | |
| 1:y | [1] | Sucesso | y = [] |
| _:y | [1,2] | Sucesso | y = [2] |
| _:y | [10,2] | Sucesso | y = [2] |
| 'X':y | ['X',2] | Erro | |
| x:y | [1,[2,3]] | Erro | |
| x | [1,2] | Sucesso | x = [1,2] |
| x:y | 1 | Erro de tipo| |
| [] | [] | Sucesso| |





## Mais de um padrão
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

próximos3 _ _ = error "Use d ou a"
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
Esta estrutura se assemelha ao `switch` de linguagens como C e Java, e tem a seguinte sintaxe, onde os padrões devem estar perfeitamente alinhados.

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
nomeMes m = case m of 1 -> "JAN"
                 m of 2 ->  "FEB"
                 m of 3 -> "MAR"
                 ...
                 m of 11 -> "NOV"
                 m of 12 -> "DEZ"
```


É importante notar que é possível aninhar `case-of`.
A função `#!hs próximos3` poderia ser reescrita assim.
Observe o alinhamento dentro do segundo case.

```hs
próximos3''' :: Int -> Char -> (Int,Int,Int)
próximos3''' n dir = case n of 0 -> (-1,0,1)
                               _ -> case dir of 'd' -> if n > 0 then (n-1,n-2,n-3) else (n+1,n+2,n+3)
                                                'a' -> if n < 0 then (n-1,n-2,n-3) else (n+1,n+2,n+3)
                                                _   -> error "Use d ou a"
```