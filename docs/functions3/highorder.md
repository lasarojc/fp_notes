# Funções de ordem superior
Quando estudamos linguagens de programação, é comum denominar como **cidadãos de primeira classe** (do inglês, *first class citizens*) as entidades que podem ser atribuídas a variáveis, passadas como parâmetro, retornadas como resultado ou operadas de forma geral.
Em Haskell, funções são cidadãs de primeira classe! Vejamos um exemplo em que uma função é associada a uma variável, o que efetivamente torna esta variável uma função!

```hs
Prelude> f x y = x + y
Prelude> g = f
Prelude> g 1 2
3
```

Outro conceito relacionado é o das **funções de ordem superior**, aquelas funções que ou recebem como parâmetro ou retornam como resultado outras funções, e que só podem existir em linguagens em que funções são cidadãs de primeira classe.
Vejamos um exemplo de uma função de ordem superior que aplica uma outra função, recebida como parâmetro, aos seus outros parâmetros.

```hs
--8<--
docs/code/higher_order.hs
--8<--
```

Observe que a função `#!hs operar` tem como primeiro parâmetro formal `#!f`, que é do tipo `#!hs (Int -> Int -> Int)`, isto é, uma função que recebe dois inteiros e retorna um inteiro.
Esta função `#!hs f` é aplicada aos demais parâmetros de de `#!hs operar` para calcular o resultado.

Funções de alta ordem são úteis em diversas situações, por exemplo, como forma de criar um comportamento configurável no seu código, e são frequentes na biblioteca da linguagem Haskell, dentre as quais destacamos `#!hs map`,  `#!hs filter`,  e diversos tipos de `#!hs fold`.

## map 
Seja uma lista de inteiros sobre a qual você queira executar diversas transformações, por exemplo, multiplicar todos os valores por 10, somar 3, testar se par ou achar o módulo por 3.
Você pode começar definindo funções que fazem transformações em um único elemento e, para cada uma, uma função correspondente para listas.

```hs
--8<--
docs/code/higher_order3.hs
--8<--
```

Se você observar as funções para as listas perceberá que todas tem uma mesma estrutura.
Graças às funções de ordem superior, você pode aproveitar este fato e criar uma função, genérica, que sirva para aplicar qualquer das transformações desejadas.

Como as funções acima, nossa função genérica recebe uma lista de um tipo `#!hs a` e resultará em uma lista de um tipo `#!hs b`.
Por iso, chamemos nossa função de `#!hs mapeie`, pois com ela mapearemos cada valor da lista de entrada para a lista de resultado. 
Além da lista de entrada, `#!hs mapeie` mas também receberá uma função com tipo `#!hs a -> b`, a ser aplicada nas transformações.

```hs
mapeie :: (a -> b) -> [a] -> [b]
mapeie f xs = [f x | x <- xs]
```

Uma vez definida a função, sua invocação é trivial.

```hs
> mapeie multiplicarPor10 [1..10]
[10,20,30,40,50,60,70,80,90,100]

> mapeie somar3 [1..10]
[4,5,6,7,8,9,10,11,12,13]

> mapeie éPar [1..10]
[False,True,False,True,False,True,False,True,False,True]

> mapeie mod3 [1..10]
[1,2,0,1,2,0,1,2,0,1]
```

A função `#!hs mapeie`, na verdade, serve para aplicar **qualquer função** que transforme `a` para `b` em uma lista de elementos do tipo `a`, resultando em uma lista de elementos do tipo `b`.
Esta função é tão útil, que já existe na biblioteca do Haskell e de qualquer linguagem de programação funcional (e mesmo outras, como Python e Java).

```hs
> :i map
map :: (a -> b) -> [a] -> [b] 	-- Defined in ‘GHC.Base’
```

## fold (redução)
Um outro procedimento recorrente sobre dados é a redução ou sumarização de um conjunto para um único valor, como é feito pelas funções `#!hs product` e `#!hs sum` nos seguintes exemplos.

```hs
> sum [1..10]
55

> product [1..3]
6
```

Vamos tentar generalizar estas duas funções usando uma função de ordem superior.
Isto é, definimos uma função de ordem superior que receba um operador, como `#!hs (+)` e `#!hs (*)`, e aplique a uma lista de inteiros calculando resumo dos elementos da lista, como o somatório ou o produtório no caso dos operadores `#!hs (+)` e `#!hs (*)`.
Comecemos por redefinir `#!hs sum` e `#!hs product` para entender melhor suas estruturas.

```hs
sum' :: [Int] -> Int
sum' [] = 0
sum' (n:ns) = n + sum' ns
```
Podemos pensar na função `#!hs sum'` como gerando uma expressão formada pelos elementos da lista mais o valor 0, separados pelo operador `+`, por exemplo, `#!hs sum' [1,2,3] = 1 + 2 + 3 + 0` e `#!hs sum' [7,9,14] = 7 + 9 + 14 + 0`.

Observe que o valor 0 é escolhido como resultado do caso base, 0, pois ele não afeta o somatório, isto é, 0 é o elemento neutro (ou identidade) da adição.

```hs
product' :: [Int] -> Int
product' [] = 1
product' (n:ns) = n * product' ns
```

No caso de `#!hs product`, embora a estrutura seja a mesma, alteramos o caso base para 1, o elemento neutro da multiplicação.
Assim, por mais estranho que possa parecer, o produtório de uma lista vazia é definido como 1.[^product] 
A expressão gerada pela função é semelhante, isto é, `#!hs product' [1,2,3] = 1 * 2 * 3 * 1` e `#!hs product' [7,9,14] = 7 * 9 * 14 * 1`.

[^product]: [Why is an empty sum 0 and an empty product 1?](https://www.johndcook.com/blog/2015/04/14/empty-sum-product/)

Podemos então generalizar a função como uma recursão em que o caso genérico é a do operador especificado e o caso base retorna o elemento neutro da operação.
O tipo da função é claro; ela deve receber um operar, que é uma função que recebe dois inteiros e retorna um inteiro, a lista de inteiros para aplicar o operador, e retornar um resultado também inteiro.
A recursão consistirá em um caso genérico, que aplica o operador à cabeça da lista e ao resultado da invocação recursivo na cauda, e de um caso base que retorne o elemento neutro da operação. Mas como saber qual é o elemento neutro?

```hs
resumir :: (Int -> Int -> Int) -> [Int] -> Int
resumir f [] = ??
resumir f (n:ns) = n `f` (resumir f ns)
```

Não nos resta alternativa senão passar o próprio elemento neutro na invocação da função, resultando na seguinte definição.

```hs
resumir :: (Int -> Int -> Int) -> Int -> [Int] -> Int
resumir f i [] = i
resumir f i (n:ns) = n `f` (resumir f i ns)

-- >>> resumir somar 0 [1..5]
-- 15

-- >>> resumir multiplicar 1 [1..5]
-- 120
```

Uma variação ligeiramente diferente, alterando a chamada recursiva, gera os mesmos resultados.

```hs
resumir' :: (Int -> Int -> Int) -> Int -> [Int] -> Int
resumir' f i [] = i
resumir' f i (n:ns) = resumir' f (i `f` n) ns

-- >>> resumir somar 0 [1..5]
-- 15

-- >>> resumir' multiplicar 1 [1..5]
-- 120
```

A diferença aqui é que em vez da recursão passar o elemento neutro adiante para ser usado no caso base, intocado, o elemento neutro é usado de cara e operado com a cabeça da lista; o resultado desta operação é que é então passado para a chamada recursiva até que, no caso base, seja retornado como o resultado da função.
Uma outra forma de ver isso é comparando as expressões geradas pelas duas declarações.
Neste caso, consideraremos operador `o` e um elemento neutro `i`, mas, desta vez, observemos também a ordem em que as operações serão efetivamente executadas, denotando a ordem por meio de parênteses.


<table>
<tr>

<td> Função </td>

<td> 

```hs 
resumir o i [1,2,3]
``` 
</td>
<td> 

```hs 
resumir' o i [1,2,3]
```
</td>
</tr>

<tr>

<td> Expressão </>

<td> 

```hs 
1 `o` (2 `o` (3 `o` i))
```
</td>
<td>

```hs
((i `o` 1) `o` 2) `o` 3
```
</td>
</tr>

<tr>

<td>Árvore </td>

<td> 

```
  `o`
 /   \
1    `o`
    /   \
   2    `o`
       /   \
      3     i
```

</td>
<td> 

```
        `o`
       /   \
     `o`    3
    /   \
  `o`   2
 /   \
i     1
```
</td>
</tr>
</table>


Pelas expressões vemos que os operadores são tratados como associativo à direita, no caso de `#!hs resumir`, e à direita, no caso de `#!hs resumir'`, e a associatividade tem implicações profundas no cálculo de uma expressão.

Considere a operação de multiplicação; por ser associativa, isto é, associativa à direita e à esquerda, $1*2*3*4 = ((1*2)*3)*4 = 1*(2*(3*4)) = 24$.

Agora considere a divisão; por ser associativa à esquerda $1/2/3/4 = ((1/2)/3)/4 = (0,5/3)/4 = 0,167/4 = 0.0146 \neq 1/(2/(3/4)) = 1/(2/0,75) = 1/2,67 = 0,375$

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

* `#!hs foldr (+) 0 [1,3,5] = 1 + (3 + (5 + (0))) = 1 + (3 + (5)) = 1 + (8) = 9`
* `#!hs foldl (+) 0 [1,3,5] = ((0 + 1) + 3) + 5 = ((1) + 3) + 5 = (4) + 5 = 9`
* ``#!hs foldr div 1 [100,10,2] =  100 `div` (10 `div` (2 `div` (1))) = 100 `div` (10 `div` (2)) = 100 `div` (5) = 20``
* ``#!hs foldl div 1 [100,10,2] =  ((1 `div` 100) `div` 10) `div` 2 = ((0) `div` 10) `div` 2 = (0) `div` 2 = 0``


Outra distinção a ser feita entre as duas funções é o fato de uma usar muito mais recursos que a outra durante a computação, mas voltaremos a discutir isso quando falarmos sobre recursão de cauda.

###### `#!hs foldl1` e `#!hs foldr1`
Para invocar as funções `#!hs foldl` e `#!hs foldr` é necessário especificar o valor a ser usado "na ponta" do cálculo, isto é, como primeiro elemento no caso de `#!hs foldl` e como último elemento no caso de `#!hs foldr`.
Se não houver um valor que naturalmente se encaixe, sempre pode-se usar ou o primeiro ou o último elemento da própria lista, desde que esta não seja vazia.
É exatamente isso que as funções `#!hs foldl1` e `#!hs foldr1` fazem.
Se usadas nas mesmas listas dos exemplos anteriores, temos os seguintes resultados.

* `#!hs foldr1 (+) [1,3,5] = 1 + (3 + (5)) = 1 + (8) = 9`
* `#!hs foldl1 (+) [1,3,5] = ((1) + 3) + 5 = (4) + 5 = 9`
* ``#!hs foldr1 div [100,10,2] =  100 `div` (10 `div` (2)) = 100 `div` (5) = 20``
* ``#!hs foldl1 div [100,10,2] =  (100 `div` 10) `div` 2 = (10) `div` 2 = 5``


###### Foldable
Todos os exemplos usados até agora foram sobre listas de números, mas a especificação das funções de redução não faz esta exigência.
Destrinchemos o exemplo de `#!hs foldr`.

```hs
> :t foldr
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
```

De forma geral, a definição diz que a função passada como primeiro parâmetro deve receber um parâmetro do tipo variável `a` e outro `b` e que o resultado deve ser do tipo `b`.
Na prática, esta definição permite que usemos funções assimétricas como parâmetro, isto é, que tenha parâmetros de tipos distintos.

!!!note "Assimetria"
     Na execução seguinte a função foi definida para testar se o número passado como parâmetro é maior que 5 e combinar a resposta com outras anteriores, na forma de um booleano.

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
Listas são `#!hs Foldable` (como um `#!hs :i []` pode rapidamente demonstrar), mas não são as únicas estruturas deste tipo. Até mesmo tipos algébricos recursivos, dependendo de suas definições, podem satisfazer este critério. 
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
       /     \        /     \
    Nó 4    Nada   Nada     Nada
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


## `#!hs filter` e `#!hs all`
A função `#!hs filter` recebe como parâmetros um predicado e uma lista e retorna como resultado um lista com todos os elementos da lista original que satisfazem ao predicado.
Já função  `#!hs all` retorna a lista dos elementos que satisfazem ao predicado.

Estas funções podem ser definidas e usadas como a seguir.

```hs
filter :: (a -> Bool) -> [a] -> [a]
filter p xs = [x | x <- xs, p x]

all :: (a -> Bool) -> [a] -> Bool
all p []     = True
all p (x:xs) = p x && all p xs


> f x = x > 10
> filter f [1..15]
[11,12,13,14,15]

> all f [1..15]
False

> all f [11..15]
True
```

Enquanto não impressionantes, estas funções podem ser usadas para demonstrar uma outra funcionalidade de Haskell.
Observe as seguintes invocações.

```hs
> filter (>10) [1..15]
[11,12,13,14,15]

> filter (10>) [1..15]
[1,2,3,4,5,6,7,8,9]

> all (>10) [1..15]
False

> all (>10) [11..15]
False
```


Os predicados definidos como `#!hs (>10)` e `#!hs (10>)` são exemplos de **Currying**.

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

## Lambda (λ)
Uma função anônima é uma função, pasmem, sem um nome, definida, por exemplo, no ato da invocação de uma função de ordem superior, e também conhecidas como abstração $\lambda$, um termo advindo do cálculo $\lambda$.
Um lambda é definido em Haskell com a seguinte sintaxe: `#!hs \v1 v2 ... vn -> exp`, onde o `#!hs \` é uma alusão ao símbolo $\lambda$, `#!hs v1 v2 ... vn` são os parâmetros formais da função, e `#!hs exp` é uma expressão sobre as variáveis.
Por exemplo, `#!hs \x -> x+1` e `#!hs \x y -> x*y` são as funções que somam 1 ao parâmetro e multiplicam os parâmetros, respectivamente; observe seus usos no exemplo a seguir.

```hs
> (\x -> x + 1) 5
6
> (\x y -> x * y) 4 5
20
```

Como qualquer outra função, lambdas podem ser associadas a variáveis e sofrerem aplicação parcial, embora usos como os do exemplo seguinte não pareçam muito úteis.

```hs
> mult = (\x y -> x * y)
> mult4 = mult 4
> mult 4 5
20
> mult4 5
20
```

A verdadeira utilidade das lambdas é no invocação de funções como `#!hs map` e `#!hs filter`, como a seguir.

```hs
> filter (\x -> x > 10) [ 1..20]
[11,12,13,14,15,16,17,18,19,20]
> map (\x -> x*2) [1..10]
[2,4,6,8,10,12,14,16,18,20]
```

Há muito mais que pode ser dito sobre abstrações lambda, por exemplo sobre como `#!hs f x = 2*x` é na verdade apenas açúcar sintático para `#!hs f = \x -> 2*x` e `#!hs f x y = x * y` é na verdade `#!hs f = \x -> \y -> x*y`, mas isso está fora do escopo deste curso.


## Operador $ (aplicação)
O operador `#!hs $` tem uma definição muito simples, dizendo apenas que ao receber uma função e um outro argumento, aplica a função ao parâmetro.

```hs
($) :: (a -> b) -> a -> b  
f $ x = f x
```

Mas se tudo o que ele faz é aplicar uma função a um parâmetro, por que é que se você olhar qualquer exemplo de código Haskell com mais do que algumas linhas de código, você encontrará o operador `#!hs $` em uso?
A resposta está na precedência e na associatividade de operador; compare-as com as de outros operadores.

```hs
> :i ($)
($) :: (a -> b) -> a -> b
infixr 0 $

> :i (+)
  ...
infixl 6 +

> :i (*)
  ...
infixl 7 *

> :i (^)
...
infixr 8 ^
```

Por ter precedência tão baixa, o operador será o último a ser executado, e por ser associativo à direita, toda a expressão à direita do operador será resolvida antes que ele seja executado, mesmo outras instâncias de `#!hs $`.
Vejamos um exemplo de como isso pode ser útil; considere a expressão a seguir.
Para forçar sua avaliação da direita para a esquerda, foi necessário o uso de diversos parênteses.

```hs
filter even (map floor (map sqrt (filter (>100) (map (2^) [1..10]))))
```

???todo "TODO"
   Imagem mostrando a resolução e evidenciando a precedência imposta pelos parênteses.

Esta mesma expressão pode ser reescrita trocando-se os parênteses por 

```hs
> filter even $ map floor $ map sqrt $ filter  (>100) $ map (2^) [1..10]
```


Outro uso interessante está na "inversão" da função `#!hs map`. Esta função aplica uma outra função, como nos exemplos acima, a todos os elementos de uma lista.
Mas como o `#!hs $`, é possível aplicar um argumento a uma lista de funções!

```hs
map f [1,2,3,4] ==> [f 1, f 2, f 3, f 4]

map ( $ x) [f1, f2, f3, f4] = [f1 $ x, f2 $ x, f3 $ x, f4 $ x]
```

Por exemplo:

```hs
> map ($ 10) [sqrt, (+4), (20-), (^2)]  
[3.1622776601683795,14.0,10.0,100.0]
```

## Operador . (composição)
Na matemática é comum falarmos em composição de funções, isto, é na aplicação de uma função ao resultado da aplicação de outra função, isto é, dadas duas funções $f$ e $g$, $(fog)(x) = f(g(x))$.
Por exemplo, sejam $f(x) = x*x$  e $g(x) = x+10$, $fog(3) = (3+10)*(3+10) = 13*13 = 169$.

Em Haskell, a composição de duas funções é feita pelo operador `#!hs .`, por exemplo:

```hs
> f x = x*x
> g x = x+10
> fog = f.g
> fog 3
169
```

###### Definição
A definição deste operador é interessante, pois este operador é uma função de ordem superior que recebe duas funções como parâmetro e retorna uma função como resultado, construída como uma abstração lambda.

```hs
(.) :: (b -> c) -> (a -> b) -> a -> c  
f . g = \x -> f (g x)
```

Uma definição alternativa seria a seguinte, onde o tipo do resultado fica mais explícito e em vez da função lambda, usamos uma notação simplificada para gerar a função resposta.

```hs
o :: (b -> c) -> (a -> b) -> (a -> c )
o f g x = f (g x)

-- >>>f x = x*x
-- >>>g x = x+10
-- >>>fog = f `o` g
-- >>> fog 3
169
```

###### Simplificação
Sobre os usos deste operador, veja o exemplo seguinte, onde várias transformações devem ser feitas sobre a mesma lista de valores.

```hs
> map negate (map sum (map tail [[1..5],[3..6],[1..7]]))
[-14,-15,-27]
```

Uma primeira observação importante sobre este código é que a lista é percorrida 3 vezes, o que certamente terá um impacto em termos de desempenho.

Este problema pode ser resolvido passando-se uma única função para o `#!hs map` que faça todas as transformações.[^learn]

[^learn]: [Learn you a Haskell: Function composition](http://learnyouahaskell.com/higher-order-functions#composition)

```hs
> map (\xs -> negate (sum (tail xs))) [[1..5],[3..6],[1..7]]  
[-14,-15,-27] 
```

Embora o uso da função lambda tenha melhorado o desempenho, uma composição das diversas funções tem o mesmo efeito, e um código mais legível.

```hs
map (negate . sum . tail) [[1..5],[3..6],[1..7]]  
[-14,-15,-27]
```