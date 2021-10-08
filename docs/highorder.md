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
A resposta é evidentemente não, pois elas aplicam a função passada como parâmetro de forma associativa à esquerda, no caso de `#!hs aplicarLista1`, e à direita, no caso de `#!hs aplicarLista2`, e associatividade tem implicações profundas no cálculo de uma expressão.
Considere a operação de multiplicação; por ser associativa, isto é, associativa à direita e à esquerda, $1*2*3*4 = ((1*2)*3)*4 = 1*(2*(3*4)) = 24$.
Agora considere a divisão; por associativa à esquerda $1/2/3/4 = ((1/2)/3)/4 = (0,5/3)/4 = 0,167/4 = 0.0146 \neq 1/(2/(3/4)) = 1/(2/0,75) = 1/2,67 = 0,375$

Esta diferença precisa ficar bem clara, pois estas duas funções são usadas muito frequentemente em Haskell, sendo definidas (de forma mais geral) na biblioteca como `#!hs foldX`.

###### `#!hs foldl` e `#!hs foldr`
*Fold* é o termo usado para descrever a operação de dobrar a lista sobre si mesma, combinando os elementos até que só sobre um valor.
Outro nome usado frequentemente é *reduce*, que inclusive nomeia funções semelhantes em outras linguagens.

Haskell tem várias versões de *fold*, mas as duas mais básicas são `#!hs foldl` e `#!hs foldr`, sendo que diferença no nome vem do fato das funções assumirem que a função passada como parâmetro é associativa a direita (como +, *) ou a esquerda (como +, *, /, -).

```hs
> foldr (+) 0 [1,3,5]
9

> foldl (+) 0 [1,3,5]
9

> foldr div 1 [100,10,2]
20
> foldl div 1 [100,10,2]
0
```

Assim, os exemplos acima podem ser vistos como se segue, evidenciando por quê o resultado da divisão inteira é diferente nas duas chamadas.

* `#!hs foldr (+) 0 [1,3,5]` $= 1 + (3 + (5 + (0))) = 1 + (3 + (5)) = 1 + (8) = 9$
* `#!hs foldl (+) 0 [1,3,5]` $= ((0 + 1) + 3) + 5 = ((1) + 3) + 5 = (4) + 5 = 9$
* `#!hs foldr div 1 [100,10,2]` $=  100 `div` (10 `div` (2 `div` (1))) = 100 `div` (10 `div` (2)) = 100 `div` (5) = 20$
* `#!hs foldl div 1 [100,10,2]` $=  ((1 `div` 100) `div` 10) `div` 2 = ((0) `div` 10) `div` 2 = (0) `div` 2 = 0$


Outra distinção a ser feita entre as duas funções é o fato de uma usar muito mais recursos que a outra durante a computação, mas voltaremos a discutir isso quando falarmos sobre recursão de cauda.

###### `#!hs foldl1` e `#!hs foldr1`
Para invocar as funções `#!hs foldl` e `#!hs foldr` é necessário especificar o valor a ser usado "na ponta" do cálculo, isto é, como primeiro elemento no caso de `#!hs foldl` e como último elemento no caso de `#!hs foldr`.
Se não houver um valor que naturalmente se encaixe, sempre pode-se usar ou o primeiro ou o último elemento da própria lista, desde que esta não seja vazia.
É exatamente isso que as funções `#!hs foldl1` e `#!hs foldr1` fazem.
Se usadas nas mesmas listas dos exemplos anteriores, temos os seguintes resultados.

* `#!hs foldr1 (+) [1,3,5]` $= 1 + (3 + (5)) = 1 + (8) = 9$
* `#!hs foldl1 (+) [1,3,5]` $= ((1) + 3) + 5 = (4) + 5 = 9$
* `#!hs foldr1 div [100,10,2]` $=  100 `div` (10 `div` (2)) = 100 `div` (5) = 20$
* `#!hs foldl1 div [100,10,2]` $=  (100 `div` 10) `div` 2 = (10) `div` 2 = 5 $


###### Foldable
Todos os exemplos usados até agora foram sobre listas de números, mas a especificação das funções de redução não faz esta exigência.
Destrinchemos o exemplo de `#!hs foldr`.

```hs
> :t foldr
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
```

De forma geral, a definição diz que a função passada como primeiro parâmetro deve receber um parâmetro do tipo variável `a` e outro `b` e que o resultado deve ser do tipo `b`.
Na prática, esta definição permite que usemos funções assimétricas comp parâmetro, isto é, que tenha parâmetros de tipos distintos.

!!!note "Assimetria"
     Por exemplo, na execução seguinte a função foi definida para testar o número passado como parâmetro é maior que 5 e combinar a resposta com outras anteriores, na forma de um booleano.

     ```hs
     > maiorque5 i b = b && (i > 5)
     > foldr maiorque5 True [1,2,3,4]
     False
     > foldr maiorque5 True [1,2,3,4,6]
     False
     > foldr maiorque5 True [6..10]
     True
     ```

A definição de `#!hs foldr` também diz que o valor inicial da função deve ser do tipo `b`, mas que a lista de valores deve encapsular um valor do tipo `a`; o encapsulamento é feito por elementos da classe `#!hs Foldable`.
Listas são `#!hs Foldable` (como um `#!hs :i []` pode rapidamente demonstrar), mas não são as únicas estruturas deste tipo. Até mesmo como tipos algébricos recursivos, dependendo de suas definições, podem satisfazer este critério. 
Por exemplo, podemos definir uma **árvore** que se "dobra" em uma travessia por em ordem dos seus elementos e usar o *folds* para encontrar o maior valor da lista, como no exemplo a seguir.

```hs
--8<--
docs/code/foldable.hs
--8<--
```

Considere a seguinte árvore:

```
                 Nó 1
             /         \
         Nó 2           Nó 3 
       /      \        /     \
  Nó 4        Nada  Nada     Nada
 /    \
Nada   Nada
```

Ou, Haskell, `#!hs arv = Nó 1    (Nó 2 (Nó 4 Nada Nada) Nada)     (Nó 3 Nada Nada)`.
Fold tem o seguinte efeito.

```hs 
> foldr (+) 0 arv
10
```

O poder desta abordagem está na genericidade na criação da árvore, que também pode armazenar outros dados e ser dobrada termos de outra operação, por exemplo `#!hs String` e `#!hs ++`:

```hs
> arv2 = Nó  "a" (Nó "b" (Nó "c" Nada Nada) Nada) (Nó "d" Nada Nada)
> foldr (++)  "" arv2
"cbad"
```

Para mais detalhes, visite http://learnyouahaskell.com/functors-applicative-functors-and-monoids#monoids

???todo "TODO"
     Expandir sobre foldable e traversable.


## all
Seja uma lista `l::[a]`, com elementos do tipo `a`. Seja um predicado `p` sobre a, isto é, `#!hs p :: a -> Bool`.
`#hs all p l` testa se o predicado `p` é válido para todos os elementos da lista `l`, por exemplo `#!hs all (>5) [6..10]` resulta em `#!hs True`.

Enquanto não impressionante, uma vez que vimos logo acima como fazer a mesma coisa usando fold, a invocação chama a atenção por causa do `#!hs (>5)`.
Este é um exemplo de **Currying**.

## Currying (aplicação parcial)
Em outras linguagens, funções são normalmente definidas com sintaxe semelhante à  `#!hs f :: (a,b) -> c`, isto é, o nome da função, seguido de um tupla de tipos de parâmetros, e de um tipo de resultado.
Isso é verdade para C, Java, Pascal, etc.
**Currying** é o processo pelo qual uma função deste tipo é transformada em uma função do tipo `#!hs f :: a -> ((b) -> c)`, ou seja, uma função que recebe somente um parâmetro e retorna uma outra função que recebe os demais parâmetros e retorna, ao o mesmo tipo que a primeira função.

O processo pode ser feito para funções mais complexas, exatamente da mesma forma, por exemplo transformando `#!hs f :: (a,b,c) -> d` em `#!hs f :: a -> ((b,c) -> d)`, e também aplicado recursivamente levando, levando a função `#!hs f :: a -> (b -> (c -> d))`, que pode ser simplificado de forma não ambígua para `#!hs f :: a -> b -> c -> d`, que é forma usada pelo Haskell.
Isso quer dizer que quando você define uma função em Haskell, voce especifica o tipo usando a forma *Curryed*, mesmo que defina a função da forma tradicional.
Acontece que o próprio Haskell, por baixo dos panos, transforma a sua função no correspondente ao tipo, o que permite que você faça coisas muito legais, como no exemplo a seguir.

```hs
> 1 + 2
3
> (1 +) 2
3
```

Esta técnica é conhecida como aplicação parcial, e serve para derivar funções mais especializadas.

```hs
> f = (1 +)
> f 2
3
```

Estas funções derivadas são como quaisquer outras funções, e podem ser usadas por exemplo em maps e filters.

```hs
> f = (1 +)
> map f [1,2,3]
[2,3,4]
> f = (==10)
> map f [1,10,2,10,3,10]
[False,True,False,True,False,True]
> filter f [1,10,2,10,3,10]
[10,10,10]
```

## Lambda

## Operador $ (aplicação)

## Operador . (composição)








Sumarização

```hs
soma []     = 0
soma (x:xs) = x + soma xs

eLógico []     = True
eLógico [x:xs] = x && eLógico xs

```

sortOn :: Ord b => (a -> b) -> [a] -> [a]#

Sort a list by comparing the results of a key function applied to each element. sortOn f is equivalent to sortBy (comparing f), but has the performance advantage of only evaluating f once for each element in the input list. This is called the decorate-sort-undecorate paradigm, or Schwartzian transform.






all
https://hoogle.haskell.org/?q=all