# Tipos de funções
Agora que já conhecemos alguns tipos, podemos ver como usá-los na definição de funções.
Para fazê-lo, devemos usar a seguinte sintaxe, onde o símbolo `#!hs ::` pode ser lido como **é do tipo** e `#!hs ->` como um separador dos tipos de parâmetros formais e do tipo da saída.

```hs
nomeFuncao :: tipo_arg1 -> ... -> tipo_argN -> tipo_saida
nomeFuncao arg1 ... argN = <definicao>
```

Por exemplo, o **protótipo** da função ``#!hs diaMes``, isto é, a definição dos tipos de entrada e saída da função, fica assim:

```hs
diasMes :: Int -> Int
diasMes m
    | m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12 = 31
    | m == 2 = 28
    | otherwise = 30
```

Esta definição pode então ser lida como "a função `#!hs diasMes` é do tipo que recebe um inteiro como entrada e retorna um inteiro como saída.

Já uma função que calcula a média de três números reais terá a seguinte definição, incluindo o protótipo 
    
```hs 
m3n :: Float -> Float -> Float -> Float
m3n a b c = (a + b + c)/2
```



!!!exercise "Exercícios"
    * Defina o protótipo da função de conversão de Fahrenheit para Celsius `#!hs f2c x = (x - 32) /1.8`
    * Descubra a quanto 100f corresponde em Célsius

    ???example "Resolução"
        ```hs
        f2c :: Float -> Float
        f2c x = (x - 32) /1.8
        ```

    * Defina o protótipo da soma de dois números inteiros `#!hs soma2int a b = a + b`
    * Aplique a função aos valores 2 e 3.
    * Aplique a função aos valores 2.0 e 3.0.
    
    ???example "Resolução"
        ```hs
        soma2int :: Int -> Int -> Int
        soma2int a b = a + b
        ```
    
    * Aplique a função aos valores 2.0 e 3.0.
    * Aplique a função aos valores 2 e 3.
    * Defina o protótipo da soma de dois números reais `#!hs soma2reais a b = a + b`
    
    ???example "Resolução"
        ```hs
        soma2reais :: Float -> Float -> Float
        ```
    * Explique a diferença de comportamento das duas últimas invocações.

    ???example "Resolução"
        As funções se comportam diferentemente, sendo que a primeira mostra um erro quando aplicada a dois números reais, porquê $Z \subset  R$ mas $R \not\subset Z$.