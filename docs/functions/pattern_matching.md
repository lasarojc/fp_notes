# Casamento de Padrões

Quando especificamos uma função, definimos sua lista de parâmetros formais, isto é, dizemos ao compilador a quais variáveis os parâmetros passados na invocação da função devem ser associados.
Por exemplo, considere a seguinte definição de uma função que soma dois números. 

```hs
soma2n x y = x + y
```

Quando a seguinte invocação acontece, o valor 3 é associado a `#!hs x` e 4 a `#!hs y`.

```hs
> soma2n 3 4
7
```

Em Haskell, contudo, é possível especificar mais do que uma lista de variáveis como parâmetros formais; é possível especifica padrões a serem casados com os parâmetros passados na invocação.
Veremos mais adiante como esse **casamento de padrões** é poderoso; por enquanto o usaremos apenas como forma de testar a igualdade dos parâmetros a constantes, como uma alternativa à definição de funções usando guardas.

Neste caso, a função é definida como uma sequência de equações em que são feitas tentativas sucessivas de casar (igualar) os argumentos passados com os parâmetros formais, na ordem das definições.
O resultado da invocação da função é dado pela **primeira** equação em que houver um casamento bem sucedido **e** todas as guardas forem satisfeitas.
Se ao final não houver casamento ou se as guardas não forem satisfeitas, ocorre um **erro de execução**.

Para começar, revisitemos a função `nomeMes`.
Usando casamento de padrões, ela ficaria assim:

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

Esta função funciona para exatamente os valores na faixa [1,12] e retornará um erro para qualquer valor fora da mesma.

```hs
> nomeMes 13
"*** Exception: scratch.hs:(112,1)-(123,18): Non-exhaustive patterns in function nomeMes
```


Para evitar o erro caso não haja um casamento, é possível usar uma definição genérica *catch-all* para casar com valores não específicos.
Por exemplo, a seguinte definição da função `#!hs fatorial` tem um tratamento especial para 0 e um caso genérico para qualquer outro número.

```hs
fatorial 0 = 1
fatorial n = n * fatorial (n-1)
```

Esta retornará imediatamente o valor 1 caso seja invocada como `#!hs fatorial 0` e recursivamente calculará o valor do fatorial caso invocada com algum valor maior que 0, por exemplo `#!hs fatorial 1`.

Mas esta definição de `fatorial` tem um problema, que aparece ao se tentar calcular o fatorial de números negativos, que são indefinidos.
Neste caso, precisamos impedir que números negativos sejam aceitos pela função, e podemos fazer isso combinando o casamento de padrões com outra forma de testar os valores dos parâmetros, como `#!hs if then else` e guardas, por exemplo.

```hs
fatorial 0 = 1
fatorial n 
    |n > 0 = n * fatorial (n-1)
    |otherwise = error "Indefinido"
```

Na execução, temos os seguintes resultados.

```hs
> fatorial 1
1

> fatorial (-1)
*** Exception: Indefinido
```