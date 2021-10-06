# Recursão de cauda
Considere a seguinte função, que calcula o somatório dos elementos em uma lista.

```hs
soma [] = 0
soma (x:xs) = x + soma xs
```

Não há nada de errado com esta definição, não é mesmo?
Veja como funciona perfeitamente.

```hs
> soma [1..10]
55
> soma [1..10000]
50005000
> 
```

Mas testemos com uma lista "um pouco" maior e teremos uma desagradável surpresa.

```hs
> soma [1..100000000]
*** Exception: stack overflow
```

Para entender o que aconteceu aqui, precisamos entender como a invocação é tratada pelo computador.
Por exemplo, no caso para resolver `#!hs soma [1,2,3,4,5]`, o seguinte acontece:

- para calcular `#!hs soma [1,2,3,4,5]`, primeiro é necessário calcular `#!hs soma [2,3,4,5]`, então a invocação recursiva é feita e somente após levar a uma resposta, a pergunta original é respondida;
- mas, para calcular `#!hs soma [2,3,4,5]`primeiro é necessário calcular `#!hs soma [3,4,5]`, e a invocação recursiva é feita, e assim por diante;
- as informações que permitem ao programa continuar a execução de uma invocação **depois** da chamada recursiva retornar, isto é, o valor da cabeça da lista na invocação atual, são "empilhadas" na memória;

![](drawings/tail_recursion.drawio#0)

Acontece que se a pilha (*stack*) cresce muito, como é o caso quando fazemos uma invocação `#!hs soma [100000000]`, ela extrapola a capacidade do processo de crescer a pilha e "transborda" (*overflow*) o espaço reservado para a pilha, que é o erro visto no exemplo anterior.
Acontece que Haskell é inteligente o suficiente para perceber se a função não tiver pendências e, neste caso, não colocar na pilha.

Para não deixar pendências, a chamada recursiva deve receber toda a informação que seja necessária para o cálculo do resultado final da função.
Esta técnica é conhecida como **recursão de cauda**, pois a chamada recursiva é a última coisa feita na função e, por isso, não deixa pendências.
Em outras palavra **o resultado da chamada recursiva é o resultado da chamada**.
No exemplo da soma, podemos reescrever a função assim:

```hs
soma 0 acc [] = acc
soma acc (x:xs) = soma (acc + x) xs
```

Como não há pilha, é possível calcular somatórios bem maiores, pois nada fica para trás, correto? 

![](drawings/tail_recursion.drawio#1)

A verdade é que o Haskell é muito preguiçoso, e não avalia o `+` enquanto não for necessário, então a computação fica na verdade assim.

![](drawings/tail_recursion.drawio#2)

Haskell nos dá, contudo, a opção de forçar o cálculo do acumulador antes de passá-lo como parâmetro para a chamada recursiva usando a função `#!hs seq`.

```hs
soma 0 acc [] = acc
soma acc (x:xs) = seq acc soma (acc + x) xs
```

O que a segunda linha do código faz é dizer ao Haskell que "primeiro avalie `#!hs acc` e **na sequência** avalie `#!hs soma (acc + x) xs`.
Outra forma, mais idiomática de escrever o mesmo código é usando `#!hs seq` de forma infixa.

```hs
soma 0 acc [] = acc
soma acc (x:xs) = acc `seq` soma (acc + x) xs
```

Felizmente você quase nunca precisará usar `#!hs seq` manualmente quando estiver escrevendo código que vá entrar em produção, isto é, que será compilado, pois poderá usar a flag `-O` para dizer ao compilador que tente encontrar onde o uso de `#!hs seq` seria apropriado, e o compilador faz um excelente trabalho neste sentido.


A técnica de passar um acumulador ou acumuladores na invocação recursiva é muito comum e deve estar sempre em sua mente.
E se você não quiser "poluir" a API com um parâmetro que só faz sentido por causa da recursão, você pode usar funções auxiliares, como a seguir.

```hs
soma l = soma' 0 l
   where soma' 0 acc [] = acc
         soma' acc (x:xs) = soma' (acc + x) xs
```

Vejamos um outro exemplo do uso de recursão de cauda, desta vez para calcular os números da sequência de Fibonacci "de baixo para cima".

```hs
--8<--
docs/code/fib1.hs
--8<--
```


###### foldl x foldr