# Avaliação Preguiçosa

Considere o seguinte código novamente.

```hs
imc p a
    | imc' <= 18.5 = "Baixo"
    | imc' <= 25.0 = "Normal"
    | imc' <= 30.0 = "Alto"
    where imc' = p / a^2
```

O uso de `#!hs where` na definição, além de melhorar a legibilidade do código, dá ao compilador Haskell a oportunidade de usar uma de suas mais importantes funcionalidades, a **avaliação preguiçosa**.
Quando a função `#!hs imc` é invocada, `#!hs imc'` não é calculada até que a primeira guarda seja testada.
Isso acontece porquê pela avaliação preguiçosa do Haskell, a avaliação acontece **apenas quando necessária**.
Para demonstrar esta funcionalidade, vamos usar a função `#!hs trace`, que imprime uma mensagem na tela a cada computação de `#!hs imc'`

```hs
import Debug.Trace(trace)

imc :: Double -> Double -> String
imc p a
    | imc' <= 18.5 = "Baixo"
    | imc' <= 25.0 = "Normal"
    | imc' <= 30.0 = "Alto"
    | otherwise = "Muito, muito alto"
    where imc' = trace "hmmm... " (p /a^2)
```

Agora atribuímos uma invocação da função a `y` e verificamos que somente quando tentamos ver o valor de de `#!hs y` é que o cálculo é realmente feito.

```hs
*Main> y = imc 90 1.8
*Main> y
"hmmm... 
Alto"
```

Além disso, a avaliação acontece **no máximo uma vez**, o que quer dizer que se a primeira guarda falha e a segunda deve ser testada, `#!hs imc'` não é recomputada, pois o Haskell se lembra do valor já calculado para os mesmos parâmetros.

```hs
import Debug.Trace(trace)

imc :: Double -> Double -> String
imc p a
    | imc' <= 18.5 = "Baixo"
    | imc' <= 25.0 = "Normal"
    | imc' <= 30.0 = "Alto"
    | otherwise = "Muito, muito alto"
    where imc' = trace "hmmm... " (p /a^2)
```

Na próxima execução, mesmo o resultado tendo sido gerado pela terceira guarda, veja que `#!hs imc'` só foi executado uma vez.

```hs
*Main> imc 90 1.8
"hmmm... 
Alto"
```

Outro princípio da avaliação preguiçosa é que se deve avaliar **somente o necessário**. 
Observe a próxima definição, onde múltiplos `#!hs where` são usados (onde todas as definições estão perfeitamente indentadas.)


```hs
imc p a
    | imc' <= baixo = "Baixo"
    | imc' <= normal = "Normal"
    | imc' <= alto = "Alto"
    where imc'   = trace "hmmm... " (p / a ^ 2)
          baixo  = trace "b" (18.5)
          normal = trace "n" (25.0)
          alto   = trace "a" (30.0)
```

Dependendo da chamada, os valores de `#!hs normal` e `#!hs alto` nunca serão avaliados.

```hs
*Main> imc 90 1.8
"hmmm... 
b
n
a
Alto"
*Main> imc 70 1.8
"hmmm... 
b
n
Normal"
```

###### Prós
A avaliação preguiçosa é imprescindível para algumas funcionalidades do Haskell, como a capacidade de expressar uma lista **infinita** no próximo trecho de código.
Se a lista tivesse que ser construída antes de se poder acessar seu início, as chamadas nunca terminariam.

```hs
*Main> x = [1,3..]
*Main> take 2 x
[1,3]
*Main> take 4 x
[1,3,5,7]
*Main> take 20 x
[1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39]
```

Mesmo quando as chamadas terminal, pode ser que o tempo de execução seja alto demais.
Por exemplo, considere a lista dos elementos da série de Fibonacci geradas por uma definição recursiva.

```hs
fibWhere 0 = 0
fibWhere 1 = 1
fibWhere n = prev + prevPrev
    where prev     = fibWhere (n - 1) 
          prevPrev = fibWhere (n - 2)
```

Se você invocar esta função para calcular `#!hs fib 5`, terá uma resposta rapidamente.
Se tentar com 20 ou 30, terá que esperar um pouco. Mas se tentar com `#!hs fib 300`, terá que esperar por muito tempo até que veja algum progresso.
Ainda assim, a seguinte chamada termina quase que imediatamente!

```hs
*Main> length [fib n | n <- [1..300]]
300

*Main> x = [fib n | n <- [1..300]]
*Main> take 1 x
[1]
*Main> take 3 x
[1,1,2]
```

Isso acontece pois para se saber o comprimento da lista não é necessário conhecer o valor dos elementos, apenas como são definidos.
O Haskell então gera uma lista de "invocações" da função `fib`, mas não chega a executar as invocações enquanto precisar.

###### Contras
Como uma moeda sempre tem duas faces, vejamos o lado ruim da avaliação preguiçosa.
Em vez de uma função custosa como a Fibonacci recursiva, se construirmos uma lista de somatórios de $2i + 2*+1, 0\leq 1 \leq 100$, ou seja, 0+1, 2+3, 4+5,...

```hs
*Main> x = [2*i + 2*i + 1 | i <- [0..100]]
*Main> take 3 x
[1,5,9]
*Main> take 5 x
[1,5,9,13,17]
*Main> last x
401
*Main> length x
101
```

Sem a avaliação preguiçosa, teríamos uma lista de 100 inteiros, sendo o maior 401, que cabe facilmente em um array de bytes.
Contudo, com a avaliação preguiçosa, temos uma lista de expressões que indicam duas multiplicações e duas somas, certamente mais espaçosas que a alternativa anterior.
A principal consequência disto é que, embora leve à economia de computação, às vezes a avaliação preguiçosa leva ao uso exagerado de espaço.
Além disso, computações pesadas invocadas em um período de pouca atividade no sistema podem ser executadas mais tarde, quando o sistema está sobrecarregado, aumentando a variabilidade do tempo de execução e dificultando a previsão de término da computação.


## Como funciona
A computação de uma função em Haskell pode ser entendida em termos da **reescrita** de expressões, da **ordem de reescrita** e do **compartilhamento** de resultados prévios.
É também baseado nestes mecanismos que a avaliação preguiçosa se torna possível.

### Reescrita
Há diferentes tipos de reescrita em Haskell.
O tipo mais simples é a **redução** em que uma expressão é substituída por outra, equivalente, mas mais simples. Por exemplo, a expressão `#!hs 2+2` pode ser **reescrita** como `#hs 4`.

Outra forma de reescrita é o **desdobramento** (*unfolding*), em que o lado esquerdo de uma equação é substituído pelo lado direito. Por exemplo, quando invocamos `#!hs 1 + head [1,2,3]`, Haskell tenta achar uma definição da função `#!hs head` que case com a invocação. Suponha que exista uma definição `#!hs head (x:_) = x`; neste caso, `#!hs head (x:_) = x` é reescrito como `1` e `#!hs head [1,2,3]` é reescrito como 1, levando a expressão inicial a ser reduzida a `#!hs 1 + 1`, onde uma redução é aplicada, levando a `#!hs 2`.

### Ordem de reescrita
Uma vez entendido que expressões podem ser reescritas, surge a dúvida do que substituir primeiro, quando houver múltiplas opções.
Por exemplo, se temos a seguinte invocação `#!hs head ((1+1):(2+2):[])` então podemos reduzir 

* `#!hs 1+1` para 1, ou 
* `#!hs 2+2` para 4, ou 
* `#!hs head ((1+1):(2+2):[])` para `#!hs (1+1)`.

Haskell escolhe sempre a redução mais "externa" (*outermost reduction* ou avaliação *call-by-name* ), no caso, a terceira opção acima.
Isso faz com que `#!hs 2+2` nunca seja reduzido, economizando recursos.
Esta abordagem não é muito comum, nem mesmo entre outras linguagens funcionais, que usam uma avaliação **gulosa** (*call-by-value* ou *eager*).

### Compartilhamento
Outro aspecto importante no processo de redução é o fato de que Haskell identifica expressões repetidas e compartilha o resultado da avaliação de uma instância com as outras.
Por exemplo, a seguinte expressão aparece no seu código, `#!hs (head x) : (head y) : (head x) : []`, então Haskell sabe que as duas aparições de `#!hs head x` levarão ao mesmo valor, graças à ausência de efeitos colaterais. 
As expressões são substituídas por uma referência para a expressão, que uma vez resolvida, é substituída na expressão original.

A avaliação preguiçosa pode ser entendida como a combinação da reescrita do tipo *call-by-name* com o compartilhamento, sendo também conhecida como *call-by-need*.


