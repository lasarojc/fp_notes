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
Tuplas são geralmente representadas usando a sintaxe `(Elem1, Elem2, ... , Elemn)`, tanto em Haskell como em diversas outras linguagens.
Assim, a função `soma2v` pode ser definida, incluindo o protótipo, como a seguir.
Observe que a função define claramente quantos elementos a tupla terá e qual a variável associada a cada uma das coordenadas de cada ponto.

```hs
soma2v :: (Int,Int) -> (Int, Int) -> (Int, Int)
soma2v (x1,y1) (x2,y2) = (x1+x2, y1+y2)
```

Para usar a função, podemos invocá-la de duas formas, com o mesmo resultado

```hs 
soma2v (3,4) (5,4)
(3,4) `soma2v` (5,4)
```

Outra forma de definir a mesma função, sem especificar o nome das variáveis, seria usando as funções `#!hs fst` e `#!hs snd` do [Prelude](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#g:3), abreviações para *first* e *second* e que retornam o primeiro e o segundo elemento de uma tupla de dois elementos, um par, respectivamente.

```hs
soma2v' :: (Int,Int) -> (Int, Int) -> (Int, Int)
soma2v' p1 p2 = ((fst p1) + (fst p2), (snd p1)+(snd p2))
```

De forma simplificada, `fst` e `snd` são definidos assim:

```hs
fst (x,_) =  x

snd (_,y) =  y
```

!!!exercise "Exercício"
    Considerando uma tupla de 4 elementos, defina 4 funções que, aos moldes de `fst` e `snd`, extraiam cada um dos 4 elementos da tupla.

    ???example "Resolução"

        ```hs
        prim (x,_,_,_) =  x
        segu (_,y,_,_) =  y
        terc (_,_,z,_) =  z
        quar (_,_,_,w) =  w
        ```

