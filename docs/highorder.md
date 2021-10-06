# Funções de mais alta ordem
Quando estudamos linguagens de programação, é comum denominar como **cidadãos de primeira classe** (do inglês, *first class citizens*) as entidades que podem ser atribuídas a variáveis, passadas como parâmetro, retornadas como resultado ou operadas de forma geral.
Em Haskell, funções são cidadãs de primeira classe! Vejamos um exemplo em que uma função é associada a uma variável, o que efetivamente torna esta variável uma função!

```hs
Prelude> f x y = x + y
Prelude> g = f
Prelude> g 1 2
3
```

Outro conceito relacionado é o das **funções de mais alta ordem**, aquelas funções que ou recebem como parâmetro ou retornam como resultado outras funções, e que só podem existir em linguagens em que funções são cidadãs de primeira classe.
Vejamos um exemplo de uma função de mais alta ordem que aplica uma outra função, recebida como parâmetro, aos seus outros parâmetros.

```hs
--8<--
docs/code/higher_order.hs
--8<--
```

Observe que a função `#!hs operar` tem como primeiro parâmetro `#!hs (Int -> Int -> Int)`, isto é, uma função que recebe dois inteiros e retorna um inteiro; ela recebe uma função como parâmetro e aplica esta função aos demais parâmetros!
Funções de alta ordem são úteis em diversas situações, por exemplo, como forma de criar um comportamento configurável no seu código.
De fato, Haskell tem diversas funções de mais alta ordem em sua biblioteca; vejamos algumas.

## map 
Seja uma lista de inteiros sobre a qual você queira executar diversas transformações, por exemplo, multiplicar todos os valores por 10, somar 3, testar se par ou achar o módulo por 3.
Você pode começar definindo funções que fazem transformações em um único elemento e, para cada uma, uma função correspondente para listas.

```hs
multiplicarPor10 :: Int -> Int
multiplicarPor10 x = x * 10

multiplicarPor10Lista :: [Int] -> [Int]
multiplicarPor10Lista l = [multiplicarPor10 e | e <- l]

soma3 :: Int -> Int
soma3 x = x + 3

soma3Lista :: [Int] -> [Int]
soma3Lista l = [soma3 e | e <- l]

éPar :: Int -> Bool
éPar x = even x

éParLista :: [Int] -> [Bool]
éParLista l = [éPar e | e <- l]

mod3 :: Int -> Int
mod3 x = x `mod` 3 

mod3List :: [Int] -> [Int]
mod3List l  = [mod3 e | e <- l]
```

Se você observar as funções para as listas perceberá que todas tem uma estrutura muito parecida.
Graças às funções de mais alta ordem, você pode aproveitar este fato e criar uma função genérica que sirva para aplicar qualquer transformação.

```hs
mapeie :: (a -> b) -> [a] -> [b]
mapeie f xs = [f x | x <- xs]

> mapeie multiplicarPor10 [1..10]
[10,20,30,40,50,60,70,80,90,100]

> mapeie soma3 [1..10]
[4,5,6,7,8,9,10,11,12,13]

> mapeie éPar [1..10]
[False,True,False,True,False,True,False,True,False,True]

> mapeie mod3 [1..10]
[1,2,0,1,2,0,1,2,0,1]
```

A função `#!hs mapeie`, na verdade, serve para aplicar qualquer função que transforme `a` para `b` em uma lista de elementos do tipo `a`, resultando em uma lista de elementos do tipo `b`.
Esta função é tão útil, que já existe na biblioteca do Haskell e de qualquer linguagem de programação funcional (e mesmo outras, como Python e Java).

```hs
> :i map
map :: (a -> b) -> [a] -> [b] 	-- Defined in ‘GHC.Base’
```

## filter
A função `#!hs filter` recebe como parâmetros um predicado e uma lista e retorna como resultado um lista com todos os elementos da lista original que satisfazem ao predicado.
Esta função pode ser definida e usada como a seguir.

```hs
> :i filter
filter :: (a -> Bool) -> [a] -> [a] 	-- Defined in ‘GHC.List’
filter p xs = [x | x <- xs, p x]

> f x = x > 10
> filter f [1..15]
[11,12,13,14,15]
```

## fold (redução)
Um procedimento recorrente sobre dados é a redução de um conjunto para um único valor, como é feito pelas funções `#!hs product` e `#!hs sum` nos seguintes exemplos.

```hs
> sum [1..10]
55

> product [1..3]
6
```

Vamos tentar generalizar estas duas funções usando uma função de mais alta ordem.
Isto é, definimos uma função de mais alta ordem que receba ou a função `#!hs (+)` ou `#!hs (*)` e aplique a uma lista de inteiros calculando o somatório ou o produtório dos elementos da lista.
Comecemos por redefinir `#!hs sum` e `#!hs product` para entender melhor suas estruturas.

```hs
sum' :: [Int] -> Int
sum' [] = 0
sum' (n:ns) = n + sum' ns

product' :: [Int] -> Int
product' [] = 1
product' (n:ns) = n * product' ns
```

Por mais estranho que possa parecer, o produtório de uma lista vazia é 1, pois assim temos um caso base para a recursão que, tanto em `#!hs sum` quanto em `#!hs product` retorna o elemento neutro e permite que a recursão retorne o resultado correto.

Façamos agora nossa função genérica.
Ela deve receber uma função que receba dois inteiros e retorne um inteiro, além da lista de inteiros para aplicar a função, e de retornar um resultado também inteiro.
Também deverá ter um caso recursivo a ser aplicado na cauda da lista.
E deverá ter um caso base que retorne o elemento neutro da operação; mas como definir o caso base?


```hs
aplicarLista :: (Int -> Int -> Int) -> [Int] -> Int
aplicarLista f [] = ??
aplicarLista f (n:ns) = n `f` (aplicarLista f ns)
```

Poderíamos testar se a função passada como parâmetro é a soma ou a multiplicação, mas você deve ter percebido que nosso objetivo é ir além destes dois operadores.
Precisamos então de outra definição, que não esbarre no problema do elemento neutro, e a solução normalmente encontrada é indicar este elemento na invocação da função.

```hs
aplicarLista1 :: (Int -> Int -> Int) -> Int -> [Int] -> Int
aplicarLista1 f i [] = i
aplicarLista1 f i (n:ns) = n `f` (aplicarLista1 f i ns)

> aplicarLista1 somar 0 [1..5]
15

> aplicarLista1 multiplicar 1 [1..5]
120
```

Enquanto correta, esta não é a única alternativa para se obter exatamente o mesmo resultado.

```hs
aplicarLista2 :: (Int -> Int -> Int) -> Int -> [Int] -> Int
aplicarLista2 f i [] = i
aplicarLista2 f i (n:ns) = aplicarLista2 f (i `f` n) ns

> aplicarLista2 somar 0 [1..5]
15

> aplicarLista2 multiplicar 1 [1..5]
120
```

Mas estas duas funções são realmente iguais?

```hs
> 1/2/3/4/5
8.333333333333333e-3
> 1/(2/(3/(4/5)))
1.875
> (((1/2)/3)/4)/5
8.333333333333333e-3

> (100 `div` (10 `div` (2 `div` 1)))
20

> ((100 `div` 10) `div` 2) `div` 1
5
```

De fato, estas duas funções são definidas (de forma mais geral) na biblioteca Haskell como `#!hs foldr` e `#!hs foldl`.

```hs
> foldr (+) 0 [1..100000]
5000050000

> foldl (+) 0 [1..100000]
5000050000

> foldr div 1 [100,10,2]
20
> foldl div 1 [100,10,2]
0
```

A diferença no nome vem do fato das funções assumirem que a função passada como parâmetro é associativa a direita (como +, *) ou a esquerda (como +, *, /, -)

Outra distinção a ser feita entre as duas funções é o fato de uma usar muito mais recursos que a outra durante a computação, mas voltaremos a discutir isso quando falarmos sobre recursão de cauda.

## all


## Lambda

## Curring (aplicação parcial)

## Operador $ (aplicação)

## Operador . (composição)








Sumarização

```hs
soma []     = 0
soma (x:xs) = x + soma xs

eLógico []     = True
eLógico [x:xs] = x && eLógico xs


dobreDireita op base []     = base 
dobreDireita op base (x:xs) = x `op` dobreDireita op base xs

dobreDireita (+) 0 [1,2,3,4,5]
dobreDireita (*) 1 [1,2,3,4,5]
```

```hs
dobreEsquerda?
```


sortOn :: Ord b => (a -> b) -> [a] -> [a]#

Sort a list by comparing the results of a key function applied to each element. sortOn f is equivalent to sortBy (comparing f), but has the performance advantage of only evaluating f once for each element in the input list. This is called the decorate-sort-undecorate paradigm, or Schwartzian transform.






all
https://hoogle.haskell.org/?q=all