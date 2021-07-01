Quando a condição testada por um guarda é de igualdade dos parâmetros com algum valor, temos uma terceira forma de definir funções que precisam testar vários casos, além de usar `if`-`then`-`else` e guardas, usando **casamento de padrões** na definição da função.
Esta opção consiste e definir a função múltiplas vezes, para os diversos casos de entrada.
Revisitando a função `nome_mes`, com esta estrutura ela ficaria assim:

```hs
nome_mes 1 = "JAN"
nome_mes 2 = "FEB"
nome_mes 3 = "MAR"
    ...
nome_mes 11 = "NOV"
nome_mes 12 = "DEZ"
```

Também que usar uma definição genérica para *catch-all*, como no exemplo da definição de `fatorial`

```hs
fatorial 0 = 1  
fatorial n = n * factorial (n - 1) 
```

!!!exercise "Exercício"
    Defina 3 funções, usando `if`-`then`-`else`, guardas e casamento de padrões, que calculem os números da série de Fibonacci, a saber

    * Fib(1) = 1
    * Fib(2) = 1
    * Fib(n) = Fib(n-1) + Fib(n-2)

    ???example "Resolução"
        ```hs
        fib_if n = if n == 1 then 1
                   else if n == 2 then 1
                   else (fib_if (n - 1)) + (fib_if (n - 2))

        fib_guard n
            | n == 1 = 1
            | n == 2 = 1
            | otherwise (fib_if (n - 1)) + (fib_if (n - 2))

        fib_pattern 1 = 1
        fib_pattern 2 = 1
        fib_pattern 3 = (fib_if (n - 1)) + (fib_if (n - 2))
        ```
        