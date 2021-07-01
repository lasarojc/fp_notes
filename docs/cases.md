Quando a condição testada por um guarda é de igualdade dos parâmetros com algum valor, temos uma terceira forma de definir funções que precisam testar vários casos, além de usar `if`-`then`-`else` e guardas, usando `case`-`of`.
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