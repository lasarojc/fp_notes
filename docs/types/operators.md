# Operadores
Operadores são apenas funções, mas funções com algumas particularidades.
Em primeiro lugar, operadores são funções binárias, isto é, que recebem exatamente dois parâmetros.
Em segundo lugar, o nome do operador é escrito apenas com símbolos (sem letras e números).
Em terceiro, operadores são naturalmente infixos, isto é, são colocados entre os dois operandos em sua invocação.

Veja o seguinte exemplo, em que se declara o operador `#!hs ******` que é essencialmente igual ao operador `#!hs +`.
Observe que o protótipo é especificado com o operador entre parênteses, mas o a definição da equação é feita usando a notação infixa.

```hs
(******) :: Int -> Int -> Int
x ****** y = x + y
```

A invocação do operador é feita normalmente.

```hs
> 1 ****** 3
4
> (******) 1 3
4
```

###### Prioridades
Quando uma expressão tem vários operadores e funções, estas são executadas na ordem e suas prioridades, a não ser que parênteses sejam usados para forçar uma ordem de avaliação.

```hs
> 1 + 2 * 3
7

> (1+2)*3
9
```

As ordens de prioridade são dadas pela definição dos operadores e podem ser verificadas usando o comando `#!hs :i` do GHCi.
No resultado da execução trecho adiante, abreviado, identificamos que os operadores de adição, multiplicação e exponenciação tem, respectivamente, prioridades 6, 7 e 8.

```hs
ghci> :i (+)
...
infixl 6 +
ghci> :i (*)
...
infixl 7 *
ghci> :i (^)
...
infixr 8 ^
```

Observe que funções normais tem prioridade maior que a dos operadores, independente de como são definidas.
No exemplo a seguir, a função double nada mais é que uma soma, mas ainda assim é avaliada primeiro na quando imersa em uma expressão contendo operadores.

```hs
> double x = x + x
> 3 * double 2 + 1
13
```