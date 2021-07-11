
Casamento de padrões

padrao e expressao

* sucesso/falha
* mesmos tipos no padrao e na expressao
* tipos
    * padrão constante
        * casa se o valor for igual à constante   
        
        | Padrão | Valor | Resultado |
        |--------|-------|-----------|
        | 10     | 10    | Casa      |
        | 10     | 20    | Não casa  |
        | 10     | 'C'   | Erro      |
        | True   | False | Não casa  |
        | False  | False | Casa      |

    * padrão variável
        * Sempre casa e a variável é associada ao valor

        | Padrão | Valor | Resultado |
        |--------|-------|-----------|
        | x     | 10    | Casa, x == 10 |
        | x     | 20    | Casa, x == 20  |
        | x     | 'C'   | Casa, x == 'C' |
        | x     | False | Casa, x == False  |
        | x     | (1,2,3) | Casa, x == (1,2,3) |

    * padrão curinga
        * Sempre casa e nenhuma associação de valor é feita

        | Padrão | Valor | Resultado |
        |--------|-------|-----------|
        | _     | 10    | Casa |
        | _     | 20    | Casa |
        | _     | 'C'   | Casa |
        | _     | False | Casa |
        | _     | (1,2,3) | Casa |

    * padrão tupla, ou melhor, uma tupla de padrões
        * Casa se
            * a tupla de padrões tiver a mesma aridade que a tupla de valores **e**
            * cada elemento da tupla de padrões casa com o elemento correspondente da tupla de valores
        * Não casa se
            * a tupla de padrões tiver a mesma aridade que a tupla de valores **e**
            * algum elemento da tupla de padrões **não** casa com o elemento correspondente da tupla de valores
        * Resulta em um erro de tipo se 
            * a tupla de padrões **não** tiver a mesma aridade que a tupla de valores **ou**
            * algum elemento da tupla de padrões resultar um erro de tipo no casamento com o elemento correspondente da tupla de valores

        | Padrão | Valor | Resultado |
        |--------|-------|-----------|
        | (x,y) | (1,2) | Casa, x == 1 e y == 2 |
        | (1,y) | (1,2) | Casa, y == 2 |
        | (_,y) | (1,2) | Casa, y == 2 |
        | ('X',y) | ('X',2) | Casa, y == 2 |
        | (x,y) | (1,(2,3)) | Casa, x == 1 e y == (2,3) |
        | (\_, (\_,y)) | (1,(2,3)) | Casa, y == 3 |
        | (x,y) | (1,(2,3),3) | Erro de tipo|
        | (1,y) | ('x',(2,3)) | Erro de tipo|


!!!todo "TODO"
    Princípio da linearidade


Vejamos algumas aplicações de casamento de padrões.

###### Definição de Funções
Como mencionado [anteriormente](../guards), casamento de padrões pode ser usado na definição de funções para simplificar testes via `if-then-else` e guardas nos parâmetros da função.

Neste caso, a função é definida como uma sequência de equações em que os parâmetros servem de padrões a serem casados com os valores da invocação.
Na invocação da função, o resultado é dado pela **primeira** equação, na ordem da declaração, em que houver o casamento padrões/valores **e** todas as guardas forem satisfeitas.
Se não houver casamento ou se as guardas não forem satisfeitas, ocorre um **erro de execução**.

Para começar, vejamos uma definição sem guardas, revisitando a função `nomeMes`.
Usando casamento de padrões, ela ficaria assim:

```hs
nomeMes 1 = "JAN"
nomeMes 2 = "FEB"
nomeMes 3 = "MAR"
    ...                -- Isso não é válido em Haskell
nomeMes 11 = "NOV"
nomeMes 12 = "DEZ"
```

Esta função funciona especificamente para valores na faixa [1,12] e retornará um erro para qualquer valor fora da mesma.
É possível usar uma definição genérica *catch-all* para casar com valores não específicos. Por exemplo, a definição da função `fatorial` tem um tratamento especial para 0 e um caso genérico para qualquer outro número.

```hs
fatorial 0 = 1
fatorial n = n * fatorial (n-1)
```

Mas esta definição de `fatorial` tem um problema, que aparece se calcular o fatorial de números negativos, que são indefinidos.
Neste caso, precisamos impedir que que números negativos sejam aceitos pela função.

```hs
fatorial 0 = 1
fatorial n 
    |n > 0 = n * fatorial (n-1)
    |otherwise = error "Indefinido"
```


!!!exercise "Exercício"
    Defina 3 funções, usando `if`-`then`-`else`, guardas e casamento de padrões, que calculem os números da série de Fibonacci, a saber

    * Fib(1) = 1
    * Fib(2) = 1
    * Fib(n) = Fib(n-1) + Fib(n-2)

    ???example "Resolução"
        ```hs
        ```
        


Vejamos outros exemplos, do pacote prelude do Haskell.
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




!!!todo "TODO"
    Rever fst e snd


!!!exercise "Exercício"
    * Seguindo os modes da definição do operador `&&`, defina o operador lógico ou `||` de três formas diferentes.
    * Seguindo os modes da definição do operador `&&`, defina o operador lógico ou `||` de três formas diferentes.















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