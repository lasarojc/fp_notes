Frequentemente em nossas funções, precisamos usar mais de uma vez um valor calculado por outra função.
No cálculo do índice de massa corporal a seguir, por exemplo, o valor do índice é recalculado a cada guarda, ou seja, pode ser calculado até 3 vezes.

```hs
imc p a
    | p / a ^ 2 <= 18.5 = "Baixo"
    | p / a ^ 2 <= 25.0 = "Normal"
    | p / a ^ 2 <= 30.0 = "Alto"
```

Para estes casos, Haskell nos dá o construto `where`, que permite fazer definições "locais" à função.

###### Where

```hs
imc p a
    | imc' <= 18.5 = "Baixo"
    | imc' <= 25.0 = "Normal"
    | imc' <= 30.0 = "Alto"
    where imc' = p / a ^ 2

```

O código fica muito mais legível desta forma.
O where pode ser até usado múltiplas vezes, bastando que todas as definições estejam perfeitamente indentadas.
No exemplo a seguir, fica fácil localizar e modificar as definições dos vários níveis de IMC. Ponto para a manutenabilidade!


```hs
imc p a
    | imc' <= baixo = "Baixo"
    | imc' <= normal = "Normal"
    | imc' <= alto = "Alto"
    where imc' = p / a ^ 2
          baixo = 18.5
          normal = 25.0
          alto = 30.0
```


`where` é especialmente útil na definição de funções recursivas, pois permite nomear as invocações, como por exemplo na definição da função que calcula os termos da sequência de Fibonacci, com e sem o uso do construto.

=== "Sem Where"
    ```hs
    fib 0 = 0
    fib 1 = 1
    fib n = fib (n - 1) + fib (n - 2)
    ```

=== "Com Where"
    ```hs
    fib 0 = 0
    fib 1 = 1
    fib n = prev + prevPrev
        where prev     = fib (n - 1) 
              prevPrev = fib (n - 2)
    ```

Neste caso pode-se argumentar que não houve ganhos com o mudança, dado que a função é muito simples.
Em funções mais complexas, contudo, os benefícios em termos de legibilidade aumentam, como na função a seguir.

```hs
--8<--
docs/code/collatz3.hs
--8<--
```

###### let-in
Um outro construto interessante e com funcionalidade parecida é o `#!hs let in`, em que se pode fazer algumas definições no `#!hs let` e que serão visíveis no `#!hs in`. Por exemplo, nas seguintes definições.

```hs
    fib 0 = 0
    fib 1 = 1
    fib n = let prev     = fib (n - 1) 
                prevPrev = fib (n - 2)
            in prev + prevPrev

areaCilindro r a = 
    let areaLateral = 2 * pi * r * a
        areaTopo = pi * r^2  
    in  areaLateral + 2 * areaTopo
```

Nestes exemplos, parece que `#!hs where` e `#!hs let in` são apenas sintaxes diferentes para o mesmo fim, criar definições locais.
Isso é verdade, mas há algumas diferenças fundamentais.
Primeiro, enquanto o as definições feitas no `#!hs where` são amarradas ao contexto da função em que estão colocadas, podendo ser vistas em quaisquer outras partes da função, por exemplo guardas, as definições feitas no `#!hs let` são muito mais localizadas, válidas apenas dentro do `#!hs in` correspondente. 
Segundo, `#!hs let in` são expressões e podem aparecer em qualquer lugar onde uma expressão é esperada.
Por exemplo, eles podem ser usados para definir constantes ou funções

```hs
*Main> 1 * (let um = 1 in um + 1) + 1
3
*Main> let quadrado x = x*x in quadrado 3
9
```


Em alguns contextos em que o escopo é muito claro, podemos omitir o `#!hs in` da definição. No GHCI, por exemplo, o fizermos, então a definição feita será válida por toda a execução do REPL, ou até que seja sobrescrita.

```hs
*Main> let quadrado x = x*x in quadrado 3
9
*Main> quadrado 3

<interactive>:25:1: error:
    Variable not in scope: quadrado :: t0 -> t
*Main> let quadrado x = x*x
*Main> quadrado 3
9
```

