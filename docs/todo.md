
## TODO

- [ ] Ativar plugin bibtex

(https://en.wikibooks.org/wiki/Haskell)


- [x] Tipos
    - [x] Haskell
        * é Fortemente tipada: sem conversão automática
        * é Statically typed: em tempo de compilação.
            * Exemplo python e tipos mutantes
    - [x] Protótipos
        - [x] Inferência de tipos.
        - [x] Especificação manual

    - [ ] Type classes
        - Comuns
            - [x] Eq
            - [x] Ord
            - [x] Enum
            - [x] Show
            - [ ] Read
            - [ ] Bounded: minBound maxBound
    - [] ] Definição
            ```hs
            class Eq a where  
            (==) :: a -> a -> Bool  
            (/=) :: a -> a -> Bool  
            x == y = not (x /= y)  
            x /= y = not (x == y) 
            ```
- [x] Tuplas

- [x] Listas
    - [x] sao homogêneas
    - [x] range operator [1..10]
    - [x] : cons
    - [x] ++ append
    - [x] !! get at index
    - [x] Pattern matching
    - [x] Compreensão de listas
    - [x] Listas infinitas
    - [ ] ordenação
        - [x] selection-sort
        - [x] quick-sort
        - [ ] Merge-sort


- [ ] Polimorfismo - funções que se aplicam a vários tipos - generics () http://learnyouahaskell.com/types-and-typeclasses)
    - [ ] type variables - http://www.decom.ufop.br/romildo/2014-1/bcc222/practices/p06-polimorfismo.pdf
    - `head :: [a] -> a`
    - `swap :: (a,b) -> (b,a)


- [x] Tipos especificados pelo usuário
        - [ ] apelidos - type
            - [x] não parametrizado
            - [ ] parametrizado (http://learnyouahaskell.com/making-our-own-types-and-typeclasses#algebraic-data-types)
        - [ ] Tipos algébricos -  data 
            * [x] Definição (https://mmhaskell.com/blog/2017/12/24/haskell-data-types-in-5-steps) (https://en.wikibooks.org/wiki/Haskell/GADT#Extending_the_language)
                - [x] não parametrizado
                - [ ] parametrizado
                - [ ] produto x soma
            * [x] Casamento de padrões
            * Exemplos
                * [ ] Maybe
                    * https://www.youtube.com/watch?v=b9FagOVqxmI
                    ```hs
                    data  Maybe a     =  Nothing | Just a  deriving (Eq, Ord, Read, Show)  
                    data  Either a b  =  Left a | Right b  deriving (Eq, Ord, Read, Show)  
                    ```
                * [ ] Either

            * [ ] Tipos recursivos
                - [ ] Listas
                - [ ] Árvores

- [x] Pattern matching
    - [x] case-of
    - [x] Declaração de funções
    - [x] as pattern - `@`


- [ ] Funções
    - [x] Equações simples
    - [x] Guarda
    - [x] where
        - https://stackoverflow.com/questions/32562614/is-something-in-the-where-clause-in-haskell-only-calculated-once
    - [x] let in 

    - [ ] Recursão
        - [x] simples
        - [x] em listas
        - [ ] de cauda
    - [ ] Currying:
    - [ ] Alta Ordem:
        - There are a number of concepts which are the main corner stones of functional programming, such as first-class and higher-order functions, pure functions, recursion, strict versus non-strict evaluation, and type systems. Higher-order functions can take other functions as arguments or return them as results. An example of a higherorder functions is an integrator or differential operator as it returns a function. Pure functions have no side effects (memory or I/O). This means that pure functions are very useful to optimize code. For example, if the result of a pure expression is not used, it can be removed without affecting the other expressions. If there is no data dependency between two pure expressions, then their order can be reversed or they can be performed in parallel and not interfere with each other. This means pure expressions are thread safe. To allow compilers to optimize code more easily in other languages, there is usually keywords you can add to tell the compiler the function is pure. Recursion functions invoke themselves, performing an operation multiple times unit the base case is reached. Common patterns of recursion can be re-factored using higher order functions. Some recursions require maintaining a stack but tail recursions can be optimized into the same code used to implement iteration in imperative languages. Functional programming that is limited to well-founded recursion with a few other constraints is called total functional programming [24]. Strict versus non-strict evaluation is a concept that divides functional languages by whether they use strict (eager) or nonstrict (lazy) evaluation. Figure 8 presents an example. Under strict evaluation the entire term and function and evaluated, meaning if any term of the expression would fail, the whole expression would fail. Under non-strict evaluation, the length function will return the value 4 since evaluation it will not attempt to evaluate the terms making p the list. The usual implementation strategy for non-strict evaluation in functional languages is graph reduction [25].
        - [ ] fold
        - [ ] filter
        - [ ] map
        - .
        - $

- [ ] Avaliação preguiçosa
    - [ ] mostrar com listas infinitas
        - [ ] a função cycle
    - [ ] Where - let in
        - https://takenobu-hs.github.io/downloads/haskell_lazy_evaluation.pdf


- [ ] Entrada e saída:
    - [ ] Show
    - [ ] Read
    - [ ] Monads

- [ ] Haskell em produção
    - [ ] Comunicação entre linguagens
    - [ ] Paralelismo e Distribuição:
        - [ ] actors

- [ ] Cálculo Lambda: 
    - https://crypto.stanford.edu/~blynn/lambda/
    - https://youtu.be/3VQ382QG-y4
    - [ ] transparência referencial - https://takenobu-hs.github.io/downloads/haskell_lazy_evaluation.pdf
    - [ ] idempotência


- [ ] Test
    - [ ] HSpec
    - [ ] quickcheck
    - [ ] quickcheck + Hspec https://hspec.github.io/quickcheck.html



Exercícios/Provas

- Fontes 
    * https://www.cantab.net/users/antoni.diller/haskell/questions/quest07.pdf
    * Exercism
    * https://www.cse.chalmers.se/edu/year/2018/course/TDA555/exam.html
    * https://www.cse.chalmers.se/edu/year/2018/course/TDA555/ex-week2.html
    * https://haskell.mooc.fi/part1#sidenote-the-.-and-operators

- [x] Tipos
    - [x] Pedra tesoura papel - usando strings
    - [x] Pedra tesoura papel - usando tipos definidos pelo usuário

- Tipos algébricos
    - Pedra tesoura papel - usando tipos algébricos
    - cartas
        - teste se bateu em algum jogo
        - ordene as cartas



* Exercícios
    * Defina uma função que dado uma String, verifique se ela é um palíndromo.

    ???example "Resolução"
        ```hs
        éPalíndromo s = s == reverse s
        ```

        ```hs
        *Main> éPalíndromo "aba"
        True
        *Main> éPalíndromo "abac"
        False
        ```

    * Um pangrama é uma frase que contem todas as letras do alfabeto. Escreva uma função que, dado uma String, verifique se é um pangrama.

    ???example "Resolução"
        ```hs
        module Pangram (isPangram) where

        import Data.Char (toLower)

        isPangram :: String -> Bool
        isPangram text = all (`elem` (map toLower text)) ['a'..'z']
        ```