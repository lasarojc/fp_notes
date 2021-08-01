Uma função é dita recursiva quando ela é **definida em termos de si mesma**.
A matemática é rica em exemplo de funções recursivas, também conhecidas como **recorrências**.

Dois exemplos bem conhecidos são o fatorial de um número...

$$
n! =
     \begin{cases}
       1          & \text{se } n = 0\\
       n * (n-1)! & \text{se } n >= 0
     \end{cases}
$$

... e a sequência de Fibonacci.

$$
F_n =
     \begin{cases}
        1                    & \text{se } n = 1\\
        1                    & \text{se } n = 2\\
        F_{n-1} + F_{n_2}    & \text{se } n > 2
     \end{cases}
$$

A recursão é muito importante na computação pois é uma estratégia de resolução de problemas, pois possibilita resolver um problema pouco a pouco.
Mas além disso, ela é especialmente importante na programação funcional, onde não há instruções de iteração como `#!c for(;;)` e `#!c while()`, comuns nas linguagens imperativas.