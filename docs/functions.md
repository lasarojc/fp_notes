# Funções
Agora que já vimos exemplos extremamente simples, vejamos alguns um pouco mais interessantes, como uma função que soma os números de `x` a `y`, inclusive.
A título de comparação, vejamos primeiro a função em linguagem C.

```c
int soma(int x, int y) {
    int total = 0;
    for (int i = x; i <= y; i++)
        total = total + i;
    return total;
}

soma(1,10);
```

Em Haskell, temos diversas opções para obter o mesmo resultado, por exemplo, usando uma função recursiva...

```Haskell
somadeate x y = if x < y
                then x + somadeate (x+1) y
                else x

somadeate 1 10
```

... ou a função já existente `#!hs sum`, que recebe uma lista de números, neste caso criada pelo operador `#!hs ..`

```hs
sum [1..10]
```

Observe que a **sintaxe do uso** das funções, contudo, é diferente; em Haskell, usa-se o nome da função, seguido dos parâmetros, com espaço os separando.
Já a definição de funções segue algumas estruturas básicas, sendo a mais simples a definição via **equação**.

### Equações

Por exemplo, consideremos uma função que soma dois números.

![Função](drawings/function.drawio#3)

Em Haskell, ela é definida simplesmente como `#!hs soma x y = x + y` e usada como `#!hs soma 3 5`.

!!!exercise "Exercício: Soma"
    * Defina a função soma e a use, de acordo com a imagem.[^replit2]

    ![Exercício](images/exe1.png)

[^replit2]: Se você se logar no Repl.it, poderá salvar seus arquivos online e editá-los de qualquer lugar, como no exemplo dado no exercício.


Como outro exemplo, definamos uma função que retorna o sucessor de um inteiro como `#!hs sucessor x = x + 1`.
Com estas duas funções, podemos inclusive compor funções, no melhor estilo $g(f(x))$, por exemplo: `#!hs sucessor (soma 3 4)` ou `#!hs sucessor (sucessor (sucessor 4))`.

!!!exercise "Exercício: Composição"
    * Defina uma função que multiplique 3 números, isto é, dados $x$, $y$ e $z$, calcule $x*y*z$.
    * Calcule $(1 + (3 * 2 * 3)) + (5 * 3 * 2)$ usando as funções de soma e multiplicação definidas.

    ???example "Resolução"
        Defina as funções

        ```hs
        soma x y = x + y
        
        mult3 x y z = x + y + z

        ```

        Invoque `#!hs soma (soma 1 (mult3 3 2 3)) (mult 5 3 2)`. Observe que os parênteses são necessários para delimitar os parâmetros de cada uma das invocações.

!!!exercise "Exercício: média de 3 números"
    * Defina uma função que calcule a média de 3 números

    ???example "Resolução"
        * `#!hs media x y z = (x + y + z)/3`

!!!exercise "Exercício: área das figuras"
    * Defina uma função o cálculo da área de cada figura geométrica.

    ![figuras](drawings/area.drawio)

    ???example "Resolução"
        ```hs
        areaQuad a = a * a
        areaRet a b = a * b
        areaCirc r = pi * r * r
        areaTri a h = (a * h )/ 2
        areaTra a b h = (a + b)/2 * h
        ```