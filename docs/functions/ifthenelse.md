Você saberia calcular o maior de dois números $a$ e $b$ sem usar um teste do tipo **se então senão**?
É bem interessante; assuma $M$ maior e $m$ menor entre $a$ e $b$.

$$\begin{eqnarray}
M &=& \frac{M + M}{2}                  \nonumber \\
    &=& \frac{M + m + M - m}{2}      \nonumber \\
    &=& \frac{M + m + (M-m)}{2}       \nonumber \\
    &=& \frac{M + n + \|M-m\|}{2}      \nonumber \\
    &=& \frac{a + b + \|a - b\|}{2}    \nonumber \\
    &=& \frac{a + b}{2} + \frac{\|a - b\|}{2}    \nonumber \\
\end{eqnarray}$$

Em outras palavras, o maior entre dois números $a$ e $b$ é igual à média dos números $a$ e $b$, mais a metade da diferença entre $a$ e $b$.
Logo, em Haskell, temos

```hs
maior x y = (x + y + (abs (x - y))) / 2
```

Estou certo de que você concordará que calcular o maior entre dois números deveria ser muito mais simples que isso, especialmente porquê naquele `abs` provavelmente há um `se-então-senão` escondido. 

$$
    abs(a,b)= 
\begin{cases}
    a,& \text{if } a\geq b\\
    b,              & \text{otherwise}
\end{cases}
$$

Mas como fazê-lo em Haskell, isto é, como testar uma condição sobre os valores para decidir a forma correta de computar o resultado?
Usando uma expressão de seleção, isto é, `#!hs if ... then ... else ...`
Esta estrutura tem a seguinte sintaxe, em que a definição True é usada caso a condição avalie para True e condição False é usada caso contrário.

```hs
if <condição> then <definição True> else <definição False>
```

Por exemplo, veja podemos calcular o maior de dois números com a seguinte definição.

```hs
maior x y = if x > y then x
                     else y
```

Observe que, diferentemente de outras linguagens em que se pode usar o `#!hs if` para decidir entre fazer ou não uma computação, o `#!hs if` do Haskell serve para decidir entre duas computações. Isto é, **o `#!hs if` deve sempre ser seguido do `#!hs then` e do `#!hs else`**.

Observe também que o `#!hs if` pode estar em qualquer parte da expressão, porquê este construto é também uma expressão.
Por exemplo, imagine que queira somar um número inteiro com o valor absoluto de outro número, sem usar o `#!hs abs`.

```hs
somaEstranha x y = x + (if y < 0 then -y else y)
```

Com esta definição, tanto `#!hs somaEstranha 1 2` quanto `#!hs somaEstranha 1 (-2)` resultam em 3. Mas este exemplo é muito estranho, então pensemos em um mais "útil", como determinar se um ano é bissexto.

* Se o ano não é múltiplo de 4, não é bissexto.
* Se é múltiplo de 4 e não é múltiplo de 100, então é bissexto.
* Se é múltiplo de 100 e não é múltiplo de 400, então não é bissexto.
* Se é múltiplo de 400, então é bissexto.

```hs
--8<--
docs/code/leapyear.hs
--8<--
```

Aninhar `#!hs if` assim pode funcionar, mas leva a estruturas complexas e difíceis de serem lidas. Há formas melhores de se lidar com múltiplas possibilidades de computação, como veremos adiante.