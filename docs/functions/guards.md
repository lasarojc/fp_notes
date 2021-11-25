Vamos definir uma função que retorne o nome do mês, dado o seu número.
Seria possível escreve esta função com `#!hs if` aninhados, assim.

```hs
nomeMes m = if m == 1 then "JAN"
             else if m == 2 then "FEB"
             else if m == 3 then "MAR"
             ...
             else if m == 11 then "NOV"
             else "DEZ"
```

Esta estrutura, contudo, pode ser simplificada com o uso **guardas**, uma opção que faz sentido quando os parâmetros de uma função podem ser classificados em grupos.
Guardas tem a seguinte sintaxe.

```hs
nomeFuncao arg1 ... argN
    | <condicao1> = <definicao1>
    | <condicao2> = <definicao2>
    ...
    | <condicaoM> = <definicaoM>
```


Especificamente, o exemplo do cálculo do nome dos meses ficaria como a seguir.

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

Embora no exemplo anterior cada condição seja muito simples, apenas um teste de igualdade, condições podem ser mais complexas, podendo incluir múltiplos testes e computações; a única condição é que resulte em um valor booleano.
Por exemplo, vejamos uma função que calcula o maior entre três números.

```hs
maiorDeTres a b c
  | a >= b && a >= c   = a
  | b >= c             = b
  | True               = c
```

###### Avaliação de cima para baixo
Observe que a avaliação das condições é feita na ordem de suas definições, ou seja, **de cima para baixo**, e isso é importante porquê alguns parâmetros podem satisfazer mais de uma condição.
Vejamos novamente o caso do cálculo de anos bissextos.
Nesta definição, se a terceira e segunda guardas fossem invertidas, o ano 1900 seria considerado bissexto, quando na verdade ele não é.

```hs
---8<---
docs/code/leapyear1.hs
---8<---
```

???exercise "Exercício"
    Escreva uma função que receba um número representando um mês, um número de 1 a 12, e retorne a quantidade de dias no mês. 
    Assuma que fevereiro sempre tem 28 dias.

    ???example "Resolução"
        ```hs
        diasMes m
            | m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12 = 31
            | m == 2 = 28
            | True   = 30
        ```


???exercise "Índice de massa corporal"
    O índice de massa corporal, IMC, é calculado como o peso dividido pelo quadrado da altura. Um IMC abaixo de 18,5, inclusive, é considerado baixo e acima de 30 é considerado alto; aqueles no intervalo são considerados normais. Defina uma função que, dados peso e altura, decida se o IMC correspondente é Baixo, Normal ou Alto.

    ???example "Resolução"
        ```hs
        imc p a
            | p / a ^ 2 <= 18.5 = "Baixo"
            | p / a ^ 2 <= 25.0 = "Normal"
            | p / a ^ 2 <= 30.0 = "Alto"
            | True = error "Não sei o que dizer"
        ```

###### `#!hs otherwise` é verdade
Em alguns dos exemplos acima, a última condição foi simplesmente `#!hs True`, que cobre **todos os outros casos**.
Embora correta, esta definição pode parecer estranha. Uma alternativa é usar `#!hs otherwise` no lugar de `#!hs True`, com exatamente os mesmos efeitos, como no seguinte exemplo.

```hs
maiorDeTres a b c
  | a >= b && a >= c   = a
  | b >= c             = b
  | otherwise          = c
```

De fato, se usarmos o GHCi para obtermos mais informações sobre `otherwise`, veremos que é uma constante, cujo valor é `#!hs True`.
Isso serve para ilustrar o poder da linguagem, que tem um conjunto reduzido de palavras reservadas e que é estendida usando suas funcionalidades básicas.

```
Prelude> otherwise
True
```