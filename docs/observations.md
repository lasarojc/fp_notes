# Observações

## Cheat sheet

https://hackage.haskell.org/package/CheatSheet-1.7/src/CheatSheet.pdf

## Exercícios da Semana 2

###### Parênteses desnecessários

* Todo `if` em um `then` e um `else`
    * Diferentemente de outras linguagens, onde o `if` é usado para determinar **se** uma computação deve acontecer e o `else` é um atalho para quando há duas opções, e portanto o `else` não é necessário, em Haskell o `if` é usado para determinar **qual** computação deve acontecer, ou seja, **sempre há duas opções**.

    === "C"
        ```c
        if (x > 3)              //If sem else: compila.
            y = "maior";        //Se x <= 3, y continua com o valor anterior, seja qual for.
        ```
    === "Haskell"
        ```hs
        let y = if x > 3 
            then "maior"  -- If sem else: não compila! Qual o valor de `y` se `x <= 3`?
        ```

    Por isso o `if` do Haskell deve ser comparado ao operador ternário do C, não com o `if`.

    === "C"
        ```c
        y = x > 3? "maior": "menor";
        ```
    === "Haskell"
        ```hs
        let y = if x > 3 then "maior" else "menor"
        ```


* `((b1+b2)/2) * h` -> `(b1+b2)/2 * h`
    * não há dúvida para o compilador que a divisão deve ocorrer primeiro, pois os operadores tem a mesma precedência e ambos são associativos à esquerda.
* `sqrt ((b*b)+ (c*c))`-> sqrt (b*b + c*c)
    * não há dúvida para o compilador que a divisão deve ocorrer primeiro, pois os operadores tem precedências diferentes.

* Informação sobre precedência e associatividade pode ser derivadas via `#!hs :info`.

    ```hs
    Prelude> :info *
    type Num :: * -> Constraint
    class Num a where
    ...
    (*) :: a -> a -> a
    ...
    infixl 7 *                              -- Infixo com associatividade a esquerda (l) e Precedência 7.
    Prelude> :info /
    type Fractional :: * -> Constraint
    class Num a => Fractional a where
    (/) :: a -> a -> a
    ...
    infixl 7 /                              -- Infixo com associatividade a esquerda (l) e Precedência 7.
    Prelude> :info +
    type Num :: * -> Constraint
    class Num a where
    (+) :: a -> a -> a
    ...
    infixl 6 +                              -- Infixo com associatividade a esquerda (l) e Precedência 6.
    ```

* `#!hs maiorDeTres a b c = if a > b && a > c then a else if b > a && b > c then b else c`
    * Difícil leitura.
    * Quebrar linhas.

* `#!hs estaoOrdenados a b c = if (a > b && b > c) then True else False`
    * `#!hs if <cond> then True else False` -> `#!hs <cond>`
    * `#!hs estaoOrdenados a b c = a > b && b > c`

* `#!hs sqrt((a^2) + (b^2))`
    * Parênteses só são necessários para deixar explícito o que é parâmetro.
    * `#!hs sqrt((a^2) + (b^2))`
        * Funções e parâmetros são separados por espaço. 
        * `#!hs sqrt ((a^2) + (b^2))`
    * `#!hs sqrt ((a^2) + (b^2))`
        * Operadores tem precedências; potência tem precedência maior que adição
        * `#!hs sqrt (a^2 + b^2)`
    * `#!hs sqrt a^2 + b^2`
        * Funções tem precedência sobre operadores.
        * `#!hs sqrt a^2 + b^2 == (sqrt a^2) + b^2`



## Exercícios da semana 3

### Erros
Para indicar uma condição de erro, use "error". Por exemplo, em uma solução eu encontrei o seguinte  `#!hs  | otherwise = (99, "error")` mas o ideal seria `#!hs |otherwise = error "mensagem de erro"`.
