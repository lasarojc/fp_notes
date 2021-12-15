# Tuplas
Nas funções vistas até agora, todos os parâmetros eram de algum tipo simples. Por exemplo, vejamos a função `soma2n` que soma 2 números:

```hs
soma2n :: Int -> Int -> Int
soma2n a b = a + b
```

Muitas vezes estes tipos simples precisam ser associados para significar algo de mais alto nível. Por exemplo, seja a função `soma2v` que soma as coordenadas `x` e `y` de 2 pontos representando dois vetores. Como poderíamos definir tal função, já que o resultado deve ter informação tanto sobre a coordenada `x` quanto `y` do resultado?

```hs
soma2v :: Int -> Int -> Int -> Int -> ██████
soma2v x1 y1 x2 y2 = █████
```

A resposta está no uso de tipos estruturados, que agregam outros tipos.
No caso, a solução ideal para o par ordenado está na forma de uma **tupla** de dois elementos.
Tuplas são geralmente representadas usando a sintaxe `#!hs (Elem1, Elem2, ... , ElemN)`, tanto em Haskell como em diversas outras linguagens.
Assim, a função `#!hs soma2v` pode ser redefinida como a seguir.
Observe que a função define claramente que as tuplas terão 2 elementos e qual a variável associada a cada um dos pontos.

```hs
soma2v :: (Int,Int) -> (Int,Int) -> (Int,Int)
soma2v p1 p2 = █████
```

Para acessar as diferentes coordenadas dentro das tuplas podemos usar as funções `#!hs fst` e `#!hs snd`,[^prelude] abreviações para *first* e *second* e que retornam o primeiro e o segundo elemento de uma tupla de dois elementos, um par, respectivamente.
Isto é, 

[^prelude]: Funções definidas no pacote [Prelude](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#g:3).

```hs
> snd (1,2)
2
> fst (3,4)
3
> fst (snd ((1,2,3),(4,5)))
4
> fst (1,2,3) <== Erro!
```

Assim, usando `#!hs fst` e `#!hs snd`, a definição da soma dos vetores fica como se segue:

```hs
soma2v :: (Int,Int) -> (Int,Int) -> (Int,Int)
soma2v p1 p2 = ((fst p1) + (fst p2), (snd p1)+(snd p2))
```

De forma simplificada, `#!hs fst` e `#!hs snd` poderiam ser definidos com a seguir.
Observe que ambas as funções esperam por uma dupla de valores e que uma variável é provida para cada elemento da dupla. Assim, como quando usamos variáveis simples, os argumentos passados para a função são casados com as variáveis e podem ser usados do lado direito da equação.

```hs
fst (x,y) =  x

snd (x,y) =  y
```

Sabendo que é possível "desmembrar" a tupla em suas componentes, podemos redefinir a função `#!hs soma2v` como se segue, muito mais intuitiva.

```hs
soma2v :: (Int,Int) -> (Int,Int) -> (Int,Int)
soma2v (x1,y1) (x2,y2) = (x1+x2, y1+y2)
```

Para usar a função, podemos invocá-la de duas formas, usando a notação prefixa ou infixa, com o mesmo resultado.

```hs 
> soma2v (3,4) (5,4)
(8,8)

>(3,4) `soma2v` (5,4)
(8,8)
```



###### Ignorando variáveis
Na prática, mas ainda de forma simplificada, as funções `#!hs fst` e `#!hs snd` são definidos assim.

```hs
fst (x,_) =  x

snd (_,y) =  y
```

Observe que o `#!hs _` é usado em substituição a um nome para **variáveis com as quais não nos importamos**, isto é, que não serão usadas no dado escopo.
A primeira vista isso poderia parecer uma forma de permitir ao compilador Haskell que otimizasse o uso de recursos, mas a verdade é que o compilador consegue muito bem identificar quais variáveis serão ou não serão usadas do lado direito da equação.
O uso de `#!hs _` é na verdade para permitir que o desenvolvedor demonstre que está ciente de que a variável não foi usada.

!!!exercise "Exercício"
    Considerando uma tupla de 4 elementos, defina 4 funções que, aos moldes de `fst` e `snd`, extraiam cada um dos 4 elementos da tupla.
    Não defina um protótipo.

    ???example "Resolução"

        ```hs
        prim (x,_,_,_) =  x
        segu (_,y,_,_) =  y
        terc (_,_,z,_) =  z
        quar (_,_,_,w) =  w
        ```

!!!exercise "Exercício"
    Escreva uma função que receba um inteiro como parâmetro e retorne uma tupla como resultado onde o primeiro elemento é um booleano que indica se o número é negativo, e o segundo elemento é o valor absoluto do número.


    ???example "Resolução"

        ```hs
        éNeg x = if x < 0 then (True, abs x) else (False, abs x)

        éNeg' x = (x < 0, abs x)
        ```


###### Tuplas são como Structs
Tuplas estão para Haskell assim como estruturas estão para outras linguagens. 
Por exemplo, imagine que se queira armazenar os dados nome, telefone, CPF e endereço de uma pessoa. 
Poderíamos convencionar que seria usado uma tupla em que cada posição corresponderia a um dos dados.
Neste caso, alguns exemplos de funções úteis são mostrados a seguir.

```hs
--8<--
docs/code/pessoa.hs
--8<--
```

Uma observação a ser feita é que, na última função, nomear a variável como `#!hs _t` tem o mesmo efeito que simplesmente `#!hs _` para o compilador, mas deixa o código mais legível.
Outra observação é que mesmo com o uso `#!hs _t`, o código fica rapidamente difícil de se ler, pois o desenvolvedor deve manter em mente qual posição corresponde a qual dado de uma pessoa. Veja o exemplo de uso das funções.

```hs
> x = fazPessoa "jose da silva" "12345" "0003003093" "Av das Couves, 14"  
> x
=> ("jose da silva","12345","0003003093","Av das Couves, 14")
> pegaNome x
=> "jose da silva"
> pegaTelefone x
=> "12345"
> y = trocaTelefone x "54321"
> y
=> ("jose da silva","54321","0003003093","Av das Couves, 14")
```

Imagine estruturas mais complexas, contendo outros dados de cada pessoa, e várias outras estruturas semelhantes, como ordens de serviço, descrição de inventários, cadastro de vendedores, etc.
Veremos adiante como definir novos tipos de dados pode facilitar o desenvolvimento e não ter que ficar se lembrando das posições dos valores dentro das tuplas.


## O tipo de um tupla
Ao definirmos a função `#!hs soma2v`, definimos que o primeiro parâmetro é uma tupla com duas componentes do tipo `#!hs Int`, ou seja, `#!hs (Int,Int)`; este é o tipo do parâmetro.
Podemos confirmar esta informação usando `#!hs :t`.

```hs
> :t soma2v
soma2v :: (Int, Int) -> (Int, Int) -> (Int, Int)
```

Para outro exemplo, considere o tipo do resultado da função `#!hs fazPessoa`, uma tupla com quatro `#!hs String`.

```hs
> x = fazPessoa "jose da silva" "12345" "0003003093" "Av das Couves, 14"
> :t x
x :: (String, String, String, String)
```

Observe que tuplas podem ter componentes de tipos diferentes. 
Por exemplo, podemos ter uma tupla `#!hs ("XMan: Primeira Turma", 2000::Int, 7.5::Float)`, cujo tipo é `#!hs (String, Int, Float)`.

```hs
> :t ("XMan: Primeira Turma", 2000::Int, 7.5::Float)
("XMan: Primeira Turma", 2000::Int, 7.5::Float) :: (String, Int, Float)
```

Os tipos das componentes de uma tupla podem ter qualquer tipo válido, inclusive outra tupla, como em
`#!hs ("XMan: Primeira Turma", (3::Int, 4::Int, 2000::Int), 7.5::Float)`, cujo tipo é `#!hs (String, (Int, Int, Int), Float)`.

```hs
> :t ("XMan: Primeira Turma", (3::Int, 4::Int, 2000::Int), 7.5::Float)
("XMan: Primeira Turma", (3::Int, 4::Int, 2000::Int), 7.5::Float) :: (String, (Int, Int, Int), Float)
```

## Ordem entre tuplas
Dado duas tuplas com mesmo tipo (mesmo tamanho e tipo de suas componentes), podemos compará-las lexicograficamente.
Isto quer dizer que uma tupla $t_1$ é menor que uma tupla $t_2$ se, considerando posições da esquerda para a direita. 
Isto é, dado tupla $t^1$ e uma tupla $t^2$, se o primeiro elemento da tupla $t^1$ é menor que o primeiro elemento da tupla $t^2$, então $t^1 < t^2$. Caso o primeiro elemento de $t^2$ seja menor, então $t^2 < t^1$. E caso os primeiros elementos sejam iguais, a avaliação é repetida para os segundos elementos e assim sucessivamente.

```hs
> (1,2) < (1,3)
True
> ('a',2) < ('b',3)
True
> ('a',2) == ('b',3)
False
> ('a',2) > ('b',3)
False
> (1,2) < (1,3,4) <== Erro!

> (1,1,1) < (1,1,1)
False
> (1,1,1) < (1,1,2)
True
> (1,1,1) < (1,2,1)
True
> (1,1,1) < (2,1,1)
True
> (1,1,1) < (0,2,2)
False
```

Um exemplo do uso desta funcionalidade é na comparação de datas, se as representarmos como tuplas com ano, mês e dia, **nesta ordem**. Neste caso, duas datas podem ser comparadas diretamente como comparação de tuplas.

```hs
> (2000,01,01) < (1999,12,12)
False
> (2000,01,01) < (2001,12,12)
True
> (2000,01,01) < (2000,01,2)
True
```

## A tupla vazia
Por completude, é preciso mencionar que tuplas podem ter qualquer aridade, inclusive zero.
Isto é, `#!hs ()` é uma tupla válida e a única instância de tuplas de aridade zero.
A utilidade desta tupla, denominada **Unit**, ficará clara mais adiante, quando falarmos sobre entrada e saída.



##  Os operadores `#!hs (,...,)`

Haskell tem várias instâncias de açúcar sintático.
Relativo a tuplas, Haskell provê uma função para a construção das mesmas, como alternativa à sintaxe usada até agora.
Por exemplo, para construir a tupla `#!hs (1,2)`, pode se usar `#!hs (,) 1 2`, e `#˜hs (,,,,)` em vez de `#!hs (1,2,3,4,5)`.
Esta possibilidade se estende pelo padrão Haskell2010 até a construção de tuplas com 15 elementos, mas o GHC vai até tuplas com cerca de 50 elementos.
