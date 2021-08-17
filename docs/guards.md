Vamos definir uma função que retorne o nome do mês, dado o seu número.
Seria possível escreve esta função com `if` aninhados, assim.

```hs
nomeMes m = if m == 1 then "JAN"
             else if m == 2 then "FEB"
             else if m == 3 then "MAR"
             ...
             else if m == 11 then "NOV"
             else "DEZ"
```

Esta estrutura, contudo, pode ser simplificada com o uso **guardas**, uma opção que faz sentido quando os parâmetros de uma função podem ser classificados em grupos.
Guardas tem a seguinte sintaxe, onde o `otherwise` é **opcional** e serve para cobrir **todos os outros casos**.

```hs
nomeFuncao arg1 ... argN
    | <condicao1> = <definicao1>
    | <condicao2> = <definicao2>
    ...
    | <condicaoM> = <definicaoM>
    | [otherwise] = <definicaoO>
```


Especificamente, o exemplo do cálculo do nome dos meses ficaria assim:

```hs
nomeMes m
    | m == 1 = "JAN"
    | m == 2 = "FEV"
    | m == 3 = "MAR"
    | m == 4 = "ABR"
    | m == 5 = "MAI"
    | m == 6 = "JUN"
    | m == 7 = "JUL"
    | m == 8 = "AGO"
    | m == 9 = "SET"
    | m == 10 = "OUT"
    | m == 11 = "NOV"
    | m == 12 = "DEZ"
    | m == 13 = "ONZ"
```

Outras observações também são importantes. Primeiro, as condições podem ser mais complexas que um simples teste, podendo incluir múltiplos testes e computações; a única condição é que retorne um booleano.
Vejamos uma função que calcula o maior entre três números.

```hs
maiorDeTres a b c
  | a >= b && a >= c   = a
  | b >= c             = b
  | otherwise          = c
```

Segundo, as condições são testadas de cima para baixo e isso é importante porquê alguns parâmetros podem satisfazer mais de uma condição.
Vejamos novamente o caso do cálculo de anos bissextos.

```hs
---8<---
docs/code/leapyear1.hs
---8<---
```

Observe que se a terceira e segunda guardas fossem invertidas, o ano 1900 seria considerado bissexto, quando na verdade ele não é.


!!!exercise "Exercício"
    Escreva uma função que receba um número representando um mês, um número de 1 a 12, e retorne a quantidade de dias no mês. 
    Assuma que fevereiro sempre tem 28 dias.

    ???example "Resolução"
        ```hs
        diasMes m
            | m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12 = 31
            | m == 2 = 28
            | otherwise = 30
        ```


!!!exercise "Índice de massa corporal"
    O índice de massa corporal, IMC, é calculado como o peso dividido pelo quadrado da altura. Um IMC abaixo de 18,5, inclusive, é considerado baixo e acima de 30 é considerado alto; aqueles no intervalo são considerados normais. Defina uma função que, dados peso e altura, decida se o IMC correspondente é Baixo, Normal ou Alto.

    ???example "Resolução"
        ```hs
        imc p a
            | p / a ^ 2 <= 18.5 = "Baixo"
            | p / a ^ 2 <= 25.0 = "Normal"
            | p / a ^ 2 <= 30.0 = "Alto"
            | otherwise = error "Não sei o que dizer"
        ```

###### `otherwise` é verdade

Uma curiosidade sobre o uso de guardas é que o caso final, que pega todas as outras opções, poderia ser definido como um guarda em que a condição é sempre verdadeira, como no seguinte exemplo:

```hs
maiorDeTres a b c
  | a >= b && a >= c   = a
  | b >= c             = b
  | True               = c
```

De fato, se usarmos o ghci para obtermos mais informações sobre `otherwise`, veremos que é uma constante, cujo valor é `#!hs True`.
Isso serve para ilustrar o poder da linguagem, que tem um conjunto reduzido de palavras chave que é estendida usando suas funcionalidades básicas.

```
Prelude> :i otherwise
otherwise :: Bool 	-- Defined in ‘GHC.Base’
Prelude> otherwise
True
```

Ainda sobre guardas, quando a condição testada é de igualdade dos parâmetros com algum valor, temos uma terceira forma de definir funções que precisam testar vários casos, além de usar `if`-`then`-`else` e guardas: [Casamento de padrões](../pattern_matching).


