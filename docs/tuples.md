# Tuplas
Nas funções vistas até agora, todos os parâmetros eram de algum tipo simples. Por exemplo, vejamos a função `soma2n` que soma 2 números:

```hs
soma2n :: Int -> Int -> Int
soma2n a b = a + b
```

Muitas vezes estes tipos simples precisam ser associados para significar algo de mais alto nível. Por exemplo, seja a função `soma2v` que soma as coordenadas `x` e `y` de 2 pontos representando dois vetores. Como poderíamos definir tal função, já que o resultado deve ter informação tanto sobre a coordenada `x` quanto `y` do resultado?

```hs
soma2v x1 y1 x2 y2 = ?
```

A resposta está no uso de tipos estruturados que agreguem outros tipos.
No caso, a solução ideal par ordenado na forma de uma **tupla** de dois elementos.
Tuplas são geralmente representadas usando a sintaxe `(Elem1, Elem2, ... , ElemN)`, tanto em Haskell como em diversas outras linguagens.
Assim, a função `soma2v` pode ser definida, incluindo o protótipo, como a seguir.
Observe que a função define claramente quantos elementos a tupla terá e qual a variável associada a cada uma das coordenadas de cada ponto.

```hs
soma2v :: (Int,Int) -> (Int, Int) -> (Int, Int)
soma2v (x1,y1) (x2,y2) = (x1+x2, y1+y2)
```

Para usar a função, podemos invocá-la de duas formas, usando a notação prefixa ou infixa, com o mesmo resultado.

```hs 
soma2v (3,4) (5,4)
(3,4) `soma2v` (5,4)
```

Outra forma de definir a mesma função, sem especificar o nome das variáveis, seria usando as funções `#!hs fst` e `#!hs snd` do [Prelude](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#g:3), abreviações para *first* e *second* e que retornam o primeiro e o segundo elemento de uma tupla de dois elementos, um par, respectivamente.
Logo, 

```hs
> snd (1,2)
2

> fst (3,4)
3

> fst (snd ((1,2,3),(4,5)))
4

> fst (1,2,3) <== Erro!
```


De forma simplificada, `fst` e `snd` poderiam ser definidos assim:

```hs
fst (x,y) =  x

snd (x,y) =  y
```

Mas na prática, ainda de forma simplificada, são definidos assim

```hs
fst (x,_) =  x

snd (_,y) =  y
```


Observe que o `_` é usado em substituição a um nome para **variáveis com as quais não nos importamos**, isto que, que não serão usadas no dado escopo.
O GHC consegue otimizar o código para não gastar recursos com mesma.



Assim, usando `#!hs fst` e `#!hs snd`, a definição da soma dos vetores fica como se segue:

```hs
soma2v' :: (Int,Int) -> (Int, Int) -> (Int, Int)
soma2v' p1 p2 = ((fst p1) + (fst p2), (snd p1)+(snd p2))
```





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
        éNeg :: Int -> (Bool,Int)
        éNeg x = if x < 0 then (True, abs x) else (False, abs x)

        éNeg' :: Int -> (Bool,Int)
        éNeg' x = (x < 0, abs x)

        ```


Tuplas estão para Haskell assim como estruturas estão para outras linguagens. 
Por exemplo, imagine que se queira armazenar os dados nome, telefone, CPF e endereço de uma pessoa. 
Poderíamos convencionar que seria usado uma tupla em que cada posição corresponderia a um dos dados.
Neste caso, alguns exemplos de funções úteis são mostrados a seguir.

```hs
--8<--
docs/code/pessoa.hs
--8<--
```


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

Uma observação a ser feita é que, na última função, nomear a variável como `_t` tem o mesmo efeito que simplesmente `_` para o compilador, mas deixa o código mais legível.
Outra observação é que mesmo com o uso `_t`, o código fica rapidamente difícil de se ler, pois o desenvolvedor deve manter em mente qual posição corresponde a qual dado de uma pessoa; imagine estruturas mais complexas, contendo outros dados de cada pessoa, e várias outras estruturas semelhantes, como ordens de serviço, descrição de inventários, cadastro de vendedores, etc.
Uma forma de simplificar o código é definir tipos associados a cada estrutura.

###### `type`

A palavra reservada `#!hs type` permite que definamos **apelidos** para tipos no Haskell. Por exemplo, podemos usar para definir o que é uma **Pessoa** e ajustar as funções definidas anteriormente assim.

```hs
--8<--
docs/code/pessoa2.hs
--8<--
```

Se perguntarmos ao Haskell qual o tipo da tupla gerada pela função `#!hs fazPessoa`, ele responderá `#!hs Pessoa`.

```hs
> :t fazPessoa
fazPessoa :: String -> String -> String -> String -> Pessoa
> p = fazPessoa "Jose" "Tel" "CPF" "End"
> :t p
p :: Pessoa
```

Podemos ir além e definir tipos usando outros tipos estruturados. Por exemplo:

```hs
--8<--
docs/code/pessoa3.hs
--8<--
```

Neste caso

```hs
> p = fazPessoa ("José","da","Silva") ("ddd","numero")  "CPF"  ("Rua da Couves","143","Brasil")
> p
(("Jos\233","da","Silva"),("ddd","numero"),"CPF",("Rua da Couves","143","Brasil"))
> :t p
p :: Pessoa
> n = pegaNome p
> n
("Jos\233","da","Silva")
> :t n
n :: Nome
```

Vejamos outro exemplo; sejam datas, tuplas de 3 inteiros: dia, mês e ano. Assim, 25 de dezembro de 1999 é `#!hs (25, 12, 1999)`.
Dado duas datas válidas, uma operação interessante é testar se uma data é menor que outra.

```hs
--8<--
docs/code/dates1.hs
--8<--
```

Mas esta função pode ser descartada com a escolha da definição de data, pois tuplas são naturalmente ordenáveis.

!!!exercise "Data Válida"
    * Defina um tipo para representar datas como tuplas.
    * Defina uma função `#!hs dataValida` que receba uma data e retorne `True` se a data for válida e `False` se for inválida.
    * Por exemplo, 38 de **onzembro** de 2021 é uma data inválida, assim como 29 de fevereiro de 2017, mas dia primeiro de Janeiro de 2000 é válida. Isto é,
        * `#!hs dataValida (38, 13, 2021) == False`
        * `#!hs dataValida (29, 02, 2017) == False`
        * `#!hs dataValida (1,1,2000) == True`

    ???example "Resolução"
        ```hs
        --8<--
        docs/code/dates2.hs
        --8<--
        ```

###### Ordem entre tuplas
Dado duas tuplas com mesmo tipo e, obviamente, **de mesmo tamanho**, elas podem se comparadas lexicográficamente.
Isto quer dizer que uma tupla $t_1$ é menor que uma tupla $t_2$ se, considerando posições da direita para a esquerda:

* dado uma posição $i$, se $t_1$ na posição $i$ é menor que $t_2$ na posição $i$ e se $\forall j < i$, $t_1$ na posição $j$ é igual a $t_2$ na posição $j$.

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

Assim, se usarmos como data uma tupla com ano, mês e dia, **nesta ordem**, então duas datas podem ser comparadas diretamente como comparação de tuplas.

```hs
--8<--
docs/code/dates3.hs
--8<--
```

###### A tupla vazia
Por completude, é preciso mencionar que tuplas podem ter qualquer aridade, inclusive zero.
Isto é, `#!hs ()` é uma tupla válida e a única instância de tuplas de aridade zero.