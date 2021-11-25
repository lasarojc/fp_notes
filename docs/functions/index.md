# Funções
No cerne da programação funcional estão as funções. 
Vejamos então como algumas funções bem simples, como uma função que soma os números de `x` a `y`,  podem ser definidas.
A título de comparação, vejamos primeiro a função em linguagem C.

```c
int soma(int x, int y) {
    int total = 0;
    for (int i = x; i <= y; i++)
        total = total + i;
    return total;
}
```

Em Haskell, temos diversas opções para obter o mesmo resultado, por exemplo, usando uma função recursiva, cuja definição deve ser clara para você, mesmo se esta for a primeira vez que vê um código em Haskell.


```Haskell
somaDeAte x y = if x < y
                then x + somaDeAte (x+1) y
                else x
```

Observe que a sintaxe do **uso** das funções, contudo, é atípica. Enquanto a função em C é invocada como `#!c soma(1,10);`, em Haskell, usa-se o nome da função, seguido dos parâmetros, com espaço os separando, isto é:[^ghci]

```hs 
> somaDeAte 1 10
55
```

A definição de funções em Haskell é feita pela especificação de uma ou mais **equações**.
Na forma mais comum de definição, do lado esquerdo da equação temos o nome da função, iniciado sempre uma **letra minúscula**, e seguido por um ou mais parâmetros formais, todos iniciados também por **letras minúsculas**. 
A convenção é que funções e parâmetros sejam nomeados usando [Camel Casing](TODO).
Já do lado direito temos definição do cálculo do resultado.

`#!hs nomeDaFunção [par1 [par2 [par2]]] = definição`

Por exemplo, consideremos uma função que soma dois números.

![Função](../drawings/function.drawio#3)

Em Haskell, ela é definida simplesmente como `#!hs soma x y = x + y` e usada como `#!hs soma 3 5`.
De forma semelhante, uma função que multiplica três números é definida como `#!hs mult3 x y z = x * y * z` e invocada como a seguir.

```hs 
> mult3 2 3 4
24
```

???exercise "Exercício: Soma"
    * Defina a função soma e use-a, de acordo com a imagem.[^replit2]

    ![Exercício](../images/exe1.png)



Como outro exemplo, definamos uma função que retorna o sucessor de um inteiro como `#!hs sucessor x = x + 1`.
Com estas funções podemos, inclusive, compor funções no melhor estilo $g(f(x))$, por exemplo: 

```hs 
> sucessor (soma 3 4)
8

> sucessor (sucessor (sucessor 4))
7
```

???exercise "Exercício: Composição"
    * Defina uma função que multiplique 3 números, isto é, dados $x$, $y$ e $z$, calcule $x*y*z$.
    * Calcule $(1 + (3 * 2 * 3)) + (5 * 3 * 2)$ usando as funções de soma e multiplicação definidas.

    ???example "Resolução"
        Defina as funções

        ```hs
        soma x y = x + y
        
        mult3 x y z = x * y * z

        ```

        Invoque `#!hs soma (soma 1 (mult3 3 2 3)) (mult3 5 3 2)`. Observe que os parênteses são necessários para delimitar os parâmetros de cada uma das invocações.

???exercise "Exercício: média de 3 números"
    * Defina uma função que calcule a média de 3 números

    ???example "Resolução"
        * `#!hs media x y z = (x + y + z)/3`

???exercise "Exercício: área das figuras"
    * Defina uma função o cálculo da área de cada figura geométrica.

    ![figuras](../drawings/area.drawio)

    ???example "Resolução"
        ```hs
        areaQuad a = a * a
        areaRet a b = a * b
        areaCirc r = pi * r * r
        areaTri a h = (a * h )/ 2
        areaTra a b h = (a + b)/2 * h
        ```



[^replit2]: Se você se logar no Repl.it, poderá salvar seus arquivos online e editá-los de qualquer lugar, como no exemplo dado no exercício.

[^ghci]: No REPL do Haskell, ghci, o prompt é terminado por `>` e a linha sem prompt tem o resultado da invocação anterior.