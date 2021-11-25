# Casamento de Padrões
Mencionamos anteriormente que quando especificamos a lista de parâmetros formais que uma função recebe, estamos dizendo ao compilador que os parâmetros passados na invocação da função devem ser associados aos parâmetros formais.
Em Haskell, este processo acontece por meio de um **casamento de padrões**.

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






## Listas
Toda lista é **ou** uma lista vazia **ou** ou um elemento cabeça seguido por uma lista cauda.
Assim, se cobrir estes dois casos em uma definição por casamento de padrões, terá coberto "todos" os casos!
Mas como? Vejamos um exemplo.

```hs
oQueHáNaCabeça :: (Show a) => [a] -> String
oQueHáNaCabeça [] = "Nada"
oQueHáNaCabeça (x:xs) = "Há " ++ x
```

* Linha 1: não se preocupe esta linha; ela apenas implica que a lista deve ser de valores convertíveis a String
* Linha 2: esta linha usa um padrão constante para testar se a lista é vazia, isto é, `#!hs []` e, neste caso, retornar a string `#!hs "Nada"` como resultado.
* Linha 3: este é o caso mais interessante, pois usa um padrão que define uma lista em que `#!hs x` é a cabeça, concatenada por `#!hs :` a uma cauda `#!hs xs`, e retorna `#!hs "Há "` seguido do valor casado com `#!hs x`.

Observe que foram usados parênteses na linha 3 para especificar o padrão, e não apenas `#!hs x:xs` como seria de se esperar.
A verdade é que o padrão é `#!hs x:xs` e os parêntesis são usados apenas para impedir que Haskell primeiro avalie `#!hs oQueHáNaCabeça x` antes de avaliar o operador `#!hs :` e o seu segundo operando.
Esta é uma das idiossincrasias de Haskell com a qual você simplesmente terá que aprender a conviver para dividir listas entre cabeça e cauda em um casamento de padrões.

Embora estes dois padrões, isto é, lista vazia e cabeça seguida de cauda, cubram todas as possibilidades de listas, não quer dizer que não haja melhores opções, dependendo do que precise extrair da lista.
Por exemplo, na próxima função há 4 casos de casamento de padrões distintos:

```hs
oQueHáNaLista :: (Show a) => [a] -> String
oQueHáNaLista [] = "Nada"
oQueHáNaLista [x] = "Só " ++ (show x)
oQueHáNaLista [x1,x2] = "Há " ++ (show x1) ++ " e " ++ (show x2)
oQueHáNaLista (x:xs) = "Há " ++ (show x) ++ " e mais um monte de coisas" 
```

* Linha 2: lista vazia;
* Linha 3: lista com exatamente um elemento, casado com `#!hs x`.
* Linha 4: lista com exatamente 2 elementos, casados com `#!hs x1` e `#!hs x2`.
* Linha 5: lista com mais de 2 elementos, em que a cabeça é casada com `#!hs x` e a cauda com `#!hs xs`.

Outra forma de obter exatamente o mesmo resultado, usando mais o operador `#!hs cons`,  seria a seguinte.

```hs
oQueHáNaLista :: (Show a) => [a] -> String
oQueHáNaLista [] = "Nada"
oQueHáNaLista (x:[]) = "Só " ++ (show x)
oQueHáNaLista (x1:x2:[]) = "Há " ++ (show x1) ++ " e " ++ (show x2)
oQueHáNaLista (x:xs) = "Há " ++ (show x) ++ " e mais um monte de coisas" 
```

Dado que strings são apenas listas de Char, estas também podem ser decompostas por casamentos de padrões.

```hs
resumo :: String -> String
resumo [] -> "Nada"
resumo [_] -> "Um"
resumo [_,_] -> "Dois"
resumo _ -> "Muitos"
```

Assim como listas podem ter elementos mais complexos que tipos primitivos, por exemplo tuplas e listas, também os casamentos de padrões aplicados a estas listas serão mais complexos.
Por exemplo, considere uma lista de três ou mais String, isto é, uma lista de lista de Char; é possível, por exemplo, selecionar a primeira letra de cada uma das três primeiras strings com a seguinte função.

```hs
iniciais :: [String] -> [Char]
iniciais [] = []
iniciais [(x:_)] = [x]
iniciais [(x:_),(y:_)] = [x,y]
iniciais ((x:_):(y:_):(z:_):_) = [x,y,z]
```


## Recursão
A recursão é essencial no processamento de listas e, de fato, muitas das funções listadas na seção anterior podem e são definidas recursivamente, como a função `maximum`:

$maximum~[1,2,3] = max~1 \left( maximum~[2,3] = max~2 \left( maximum~[3] = 3 \right)        \right)$

Vejamos algumas definições.[^alt]

[^alt]: Todas as funções definidas a seguir tem nome terminado em `\`` para evitar colisão com as funções padrão.

!!!example "maximum"
    ```hs
    maximum' [] = error "lista vazia"  
    maximum' [h] = h
    maximum' (h:t) = max h (maximum' t)
    ```

!!!example "length"
    ```hs
    length' :: [a] -> Int
    length' [] = 0
    length' (x:xs) = 1 + length' xs
    ```

!!!example "last"
    ```hs
    last' :: [a] -> a
    last' [] = error "List is empty"
    last' [x] = x
    last' (_:xs) = last' xs
    ``` 

!!!example "reverse"
    ```hs
    reverse' :: [a] -> [a]
    reverse' [] = []
    reverse' (x:xs) = reverse' xs ++ [x]
    ```

!!!example "replicate"
    ```hs
    replicate' 0 e = []
    replicate' x e = e:replicate' (x-1) e
    ```

!!!example "zip"
    ```hs
    zip' [] _ = []
    zip' _ [] = []
    zip' (x:xs) (y:ys) = (x,y) : zip' xs ys
    ```

!!!example "elem"
    ```hs
    elem' _ [] = False
    elem' e (x:xs) = e == x || elem' e xs
    ```

!!!example "++"
    ```hs
    maisMais :: [a] -> [a] -> [a]
    maisMais [] [] = []
    maisMais [] y = y
    maisMais (x:xs) y = x : maisMais xs y
    ```

!!!exercise "Exercício"
     * Defina a função `take`
     * Defina a função `drop`
     * Defina a função `união` que recebe duas listas sem repetições retorna a concatenação das listas, sem repetições, usando recursividade.
     * Defina a função `união` que recebe duas listas sem repetições retorna a concatenação das listas, sem repetições, usando compreensão de listas.

!!!exercise "Ordenação"
    * Selection-sort (https://www.youtube.com/embed/Ns4TPTC8whw)
        * Defina uma função `fr` que receba um inteiro `i` e uma lista de inteiros `l` e retorne a lista `l` sem a primeira ocorrência de `i` em `l`.
        * Defina uma função `fm` que receba uma lista de inteiros `l` e retorne o menor inteiro da lista.
        * Defina uma função `fs` que receba uma lista de inteiros `l`, escolha o menor inteiro `m` de `l` e retorne `m` concatenado a cabeça da lista gerada por `fs (fr m l)`

    * Merge-Sort (https://www.youtube.com/embed/XaqR3G_NVoo)
        * Defina uma função `fd` que receba uma lista e retorne suas duas metades em uma dupla: `#!hs metade [1..11] = ([1,2,3,4,5],[6,7,8,9,10,11])`
        * Defina uma função `fu` que receba duas listas ordenadas e retorne uma lista ordenada com a união das listas `#!hs união [1,3,5,7] [2,4,6,7] = [1,2,3,5,6,7,7]`
        * Defina uma função `fm` que receba uma lista, divida-a na metade usando `fd`, aplique `fm` recursivamente em cada metade, e calcule a união das listas resultantes usando `fu`.

    * Quick-sort
        * Defina uma função `fp` que receba uma lista `l` de inteiros e retorne retorne uma tripla `(p,m1,m2)` em que 
            * `p` é o primeiro elemento da lista `l`
            * `m1` é a lista dos elementos em `l` menores ou iguais a `p`, exclusive
            * `m2` é a lista dos elementos em `l` maiores que `p`
        * Defina uma função `fq` que recebe uma lista `l`, calcule `(p,m1,m2) = fp l` e retorne `fq m1` concatenado a `p` concatenado a `fq m2`.