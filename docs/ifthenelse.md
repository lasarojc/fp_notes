Você como saberia calcular o maior de dois números $a$ e $b$ sem usar um teste do tipo **se então senão**?
É bem interessante; assuma $M$ maior e $m$ menor entre $a$ e $b$.

$$\begin{eqnarray}
M &=& (M + M)/2                  \nonumber \\
    &=& (M + (m + (M-m)))/2      \nonumber \\
    &=& (M + m + (M-n))/2        \nonumber \\
    &=& (M + n + \|M-n\|)/2      \nonumber \\
    &=& (a + b + \|a - b\|)/2    \nonumber
\end{eqnarray}$$

Logo, em Haskell, temos

```hs
   maior x y = (x + y + (abs (x - y))) / 2
```

Estou certo de que você concordará que calcular o maior entre dois números deveria ser muito mais simples que isso, especialmente porquê naquele `abs` provavelmente há um `if` escondido. 

$$
    f(a,b)= 
\begin{cases}
    a,& \text{if } a\geq b\\
    b,              & \text{otherwise}
\end{cases}
$$

Mas como fazê-lo em Haskell, isto é, como testar uma condição sobre os valores para decidir a forma correta de computar o resultado?
Usando uma expressão de seleção, isto é, `#!hs if ... then ... else ...` Veja o exemplo da escolha do maior número.

```hs
   maior x y = if x > y then x
                        else y
```

Observe que, diferentemente de outras linguagens em que se pode usar o `if` para decidir entre fazer ou não uma computação, o `if` do Haskell serve para decidir entre duas computações. Isto é, o `if` deve sempre ser seguido do `then` **e** do `else`.

Observe também que o `if` pode estar em qualquer parte da expressão. 
Por exemplo, imagine que queira somar um número inteiro com o valor absoluto de outro número, sem usar o `abs`.

```hs
somaEstranha x y = x + (if y < 0 then -y else y)
```

Com esta definição, tanto `somaEstranha 1 2` quanto `somaEstranha 1 (-2)` resultam em 3. Mas este exemplo é muito estranho, então pensemos em um mais útil.

!!!exercise "Exercício"
    Implemente uma função que calcule se um ano é bissexto sabendo que:

    * Se o não é múltiplo de 4, não é bissexto.
    * Se é múltiplo de 4 e não é múltiplo de 100, então é bissexto.
    * Se é múltiplo de 100 e não é múltiplo de 400, então não é bissexto.
    * Se é múltiplo de 400, então é bissexto.

    ???example "Resolução"
        ```hs
        --8<--
        docs/code/leapyear.hs
        --8<--
        ```

Aninhar `if` assim pode funcionar, mas leva a estruturas estranhas e difíceis de serem lidas. Há formas melhores de se lidar com múltiplas possibilidades de computação.