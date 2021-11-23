# Polimorfismo

Observe o funcionamento da função `#!hs head` nos seguintes exemplos.

```hs
> head [(1,2),(3,4),(5,6)]
(1,2)
> head [1,2,3,4]
1
> head "abcd"
'a'
```

Independentemente do tipo dos elementos da lista e, portanto, da lista, `#!hs head` consistentemente retorna o primeiro elemento da lista passada como parâmetro.
Isso acontece porquê a função `#!hs head` é o que chamamos de **polimórfica**, literalmente, com muitos formas, e pode atuar sobre listas de diversos tipos.

Funções polimórficas são declaradas usando **variáveis de tipo** em vez de tipos específicos para os parâmetros, como fizemos até agora.
A função `#!hs head`, por exemplo, tem declaração de tipo `#!hs head :: [a] -> a`,[^parametric] onde `#!hs a` é a variável de tipo.
Esta declaração implica que a função recebe como parâmetro uma lista de elementos de um tipo qualquer `#!hs a` e que retorna um elemento **do mesmo tipo** `#!hs a`.


Várias variáveis de tipos podem aparecer em uma mesma declaração, como em `#!hs snd :: (a,b) -> b`, que define que o resultado da função é do mesmo tipo do segundo elemento da dupla passada como parâmetros, ou `#!hs zip :: [a] -> [b] -> [(a,b)]`, que combina o tipo dos parâmetros de entrada em uma tupla dos mesmos tipos no resultado da função.
Observe que variáveis de tipo **devem começar com uma letra minúscula** e, geralmente, são recebem nomes `#!hs a,b,c...`, mas fora isso, podem ter qualquer nome, o que literalmente permite que funções tenham uma infinitude de nomes.

Declarações também podem misturar variáveis de tipo com tipos normais, como em `#!hs length :: [a] -> Int`, que define que o comprimento de uma lista de qualquer tipo será sempre do tipo `#!hs Int`, e `#!hs take :: Int -> [a] -> [a]`.

Enquanto algumas funções tem variáveis de tipo completamente livres, isto é, podem ser instanciadas com qualquer tipo, outras impõem certas restrições nos tipos na forma de **classes de tipo**.

Por exemplo, a função `#!hs sort` recebe uma lista de dados e retorna uma lista com os mesmos dados, ordenados.
Para implementar a ordenação, é preciso que os elementos da lista possam ser comparados uns com os outros, o que é determinado pela pertinência do tipo à classe de tipos `#!hs Ord`. 
A declaração da função fica então assim: `#!hs sort :: Ord a => [a] -> [a]`.[^adhoc]


Uma mesma variável de tipo pode pertencer a múltiplas classes e diferentes variáveis podem pertencer a classes diferentes, como nas seguintes declarações:

```hs
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort (filter (<= x) xs) ++ [x] ++ quickSort (filter (> x) xs)

sortAndPrint :: (Show a, Ord a) => [a] -> [String]
sortAndPrint l = map show (quickSort l)


> quickSort [10,9..1]
[1,2,3,4,5,6,7,8,9,10]
> sortAndPrint [10,9..1]
["1","2","3","4","5","6","7","8","9","10"]
```


[^parametric]: Este tipo de polimorfismo, em que o tipo não tem qualquer restrição, é denominado **paramétrico**.
[^adhoc]: Este tipo de polimorfismo é denominado *ad-hoc*.
