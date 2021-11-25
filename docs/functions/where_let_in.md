# Definição Local de Funções

Frequentemente em nossas funções precisamos usar mais de uma vez um valor calculado por outra função.
No cálculo do índice de massa corporal a seguir, por exemplo, o cálculo do índice `#!hs p / a ^ 2` é feito em três locais diferentes.

```hs
imc p a
    | p/a^2 <= 18.5 = "Baixo"
    | p/a^2 <= 25.0 = "Normal"
    | p/a^2 <= 30.0 = "Alto"
```

Uma alternativa que tornaria o código mais manutenível seria especificar o cálculo em uma função auxiliar, como no seguinte código.

```hs
aux p a = p/a^2

imc p a
    | aux p a <= 18.5 = "Baixo"
    | aux p a <= 25.0 = "Normal"
    | aux p a <= 30.0 = "Alto"
```

Entretanto, a `#!hs aux` é visível para todas as outras funções especificadas no mesmo arquivo, mesmo que não tenham nenhum interesse na mesma. Além disso, se outras funções auxiliares forem necessárias, terão cada uma que usar um nome único.
Para estes casos, Haskell nos permite fazer definições "locais" das funções auxiliares, isto é, que serão visíveis apenas dentro do escopo da função em que são definidas.
Isso pode ser feito usando `#!hs where` e `#!hs let in`.

###### `#!hs where`
As definições usando `#!hs where` são feitas após o bloco em que estas definições são usadas, como no seguinte exemplo.

```hs
imc p a
    | imc' p a <= 18.5 = "Baixo"
    | imc' p a <= 25.0 = "Normal"
    | imc' p a <= 30.0 = "Alto"
    where imc' p' a' = p'/a'^2
```

Esta definição pode ser simplificada usando o fato de que todos **os parâmetros formais da função externa são também visíveis dentro da função interna**.

```hs
imc p a
    | imc' <= 18.5 = "Baixo"
    | imc' <= 25.0 = "Normal"
    | imc' <= 30.0 = "Alto"
    where imc' = p/a^2
```

Outro aspecto importante do uso de `#!hs where` é que várias definições podem ser feitas, bastando que todas estejam perfeitamente indentadas.
No exemplo a seguir, fica fácil localizar e modificar as definições dos vários níveis de IMC. Ponto para a manutenabilidade!


```hs
imc p a
    | imc' <= baixo = "Baixo"
    | imc' <= normal = "Normal"
    | imc' <= alto = "Alto"
    where imc' = p/a^2
          baixo = 18.5
          normal = 25.0
          alto = 30.0
```


`where` é especialmente útil na definição de funções recursivas, pois permite nomear as invocações, como por exemplo na definição da função que calcula os termos da sequência de Fibonacci.

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

Finalmente, note que `#!hs where` pode ser aninhado, isto é, definições locais podem também ter suas próprias definições locais.


###### `#!hs let ... in`
`#!hs let ... in` oferece uma outra alternativa em Haskell para fazer definições locais.
Neste caso, definições que se seguem ao `#!hs let` são visíveis na expressão definida no `#!hs in`, como nas seguintes definições.

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

Nestes exemplos, parece que `#!hs where` e `#!hs let in` são apenas sintaxes diferentes para o mesmo fim, criar definições locais, mas há algumas diferenças fundamentais.
Primeiro, enquanto o as definições feitas no `#!hs where` são amarradas ao contexto da função em que estão colocadas, podendo ser vistas em quaisquer outras partes da função, por exemplo nas guardas do cálculo do `#!hs imc`, as definições feitas no `#!hs let` são muito mais localizadas, válidas apenas dentro do `#!hs in` correspondente. 
Segundo, `#!hs let ... in` são expressões e podem aparecer em qualquer lugar onde uma expressão é esperada.
Por exemplo, eles podem ser usados para definir constantes ou funções literalmente no meio de uma expressão.

```hs
> 2 * (let um = 1 in um + 2) + 3
9
> let quadrado x = x*x in quadrado 3
9
```

Em alguns contextos em que o escopo é muito claro, podemos omitir o `#!hs in` da definição. No GHCi, por exemplo, se o fizermos então a definição feita será válida por toda a execução do REPL, ou até que seja sobrescrita.

```hs
> let quadrado x = x*x in quadrado 3
9
> quadrado 3

<interactive>:25:1: error:
    Variable not in scope: quadrado :: t0 -> t
> let quadrado x = x*x
> quadrado 3
9
```

