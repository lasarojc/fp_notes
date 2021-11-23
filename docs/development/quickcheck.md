Escrever casos de teste individuais é uma tarefa tediosa e à medida que nossas funções  ficam mais complexas, gastaríamos mais e mais tempo escrevendo testes.
Uma ferramenta que nos ajuda a fazer isso é a biblioteca **QuickCheck**.
Para entender como usar a QuickCheck, considere as duas seguintes funções.

```hs
celsius2fahrenheit c = c * 9 / 5 + 32 

fahrenheit2celsius f = (f - 32) * 5/9
```

Elas são obviamente inversas uma da outra, o que quer dizer que se eu aplicar um valor qualquer à primeira função e então aplicar o resultado na segunda, eu deveria recuperar o valor original.
Por exemplo, se o valor qualquer é 100, `#!hs 100 == fahrenheit2celsius (celsius2fahrenheit 100)`.
Pois a QuickCheck nos permite escrever exatamente este tipo de afirmação e tê-la testada automaticamente para um conjunto de valores aleatórios. 
Obviamente, mesmo se o código passar nos testes gerados pelo QuickCheck, não quer dizer que outros valores não resultariam em erros.
Contudo, nossa confiança na corretude aumentaria.

Para testar as funções acima escreveríamos então a propriedade como uma função que retorna um booleano.
A propriedade tem um nome iniciado em `#!hs prop_`, mas isso é apenas uma convenção.
A definição recebe como parâmetro um valor `c`, converte para Celsius e de volta para Fahrenheit, e confere se o valor resultante é igual ao valor de entrada.
Observe que o resultado da função é o resultado da comparação com o valor inicial, o que é equivalente mas muito mais limpo do que fazer um teste do tipo `#!hs if Condição then True else False`.

```hs 
prop_C2f2C c = fahrenheit2celsius (celsius2fahrenheit c) == c
```

Para usar a biblioteca, precisamos importá-la, no início do arquivo, com um `#!hs import Test.QuickCheck`.
Assim, o arquivo ficará assim.

```hs 
---8<---
docs/code/f2c2f.hs
---8<---
```

Para testar o código, execute o ghci carregando o QuickCheck. A forma mais simples de fazê-lo é usando stack: `stack ghci --package QuickCheck`.
Agora carregue seu programa,  usando `:l f2c2f` no meu caso e execute o teste

```hs
> quickCheck prop_C2f2C
*** Failed! Falsified (after 2 tests and 4 shrinks):
-0.1
```

Ooops! O teste falhou para o valor -0.1. Mas por quê? Vejamos cada função isoladamente.

```
> celsius2fahrenheit (-0.1)
31.82
> fahrenheit2celsius 31.82
-9.999999999999984e-2
```

Os valores são próximos, mas não são iguais, por causa de problemas de arredondamento causados pela imprecisão dos tipos utilizados.
O problema está no teste, pois é impossível para o computador representar certos valores e estes erros de aproximação ocorrerão.
Assim, uma estratégia melhor é definir um operador **quase igual**, para comparar valores com pequenos erros.

```hs 
---8<---
docs/code/f2c2f2.hs
---8<---
```

Com esta nova definição, nosso teste agora passa com sucesso.

```
> quickCheck prop_C2f2C
+++ OK, passed 100 tests.
```

###### HSpec + Quickcheck

???todo "TODO"
    Descrever o uso de lambda no seguinte código.

```hs
import Test.Hspec        (Spec, it, shouldBe, describe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)
import Test.QuickCheck

import Exercise (hello, olá, soma)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = False} specs

specs :: Spec
specs = do
    describe "soma" $ do
        prop "soma números" $
          \x y -> soma x y `shouldBe` x + y
```


https://hspec.github.io/quickcheck.html