# HSpec [^hspec]
A medida que a complexidade do software aumenta, aumenta também os testes a serem executados no mesmo, tornando a execução manual impraticável.
HSpec é um framework que possibilita escrever e executar testes unitários de funções de forma automatizada.
O *framework* provê uma linguagem de domínio específico para escrever os testes também em Haskell, como demonstrado no seguinte arquivo `test/Tests.hs`.

```hs
import Test.Hspec        (Spec, it, shouldBe, describe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Exercise (hello, olá, soma)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = False} specs

specs :: Spec
specs = do
    describe "hello" $ do
        it "retorna a string `Hello, Haskell!`" $
          hello `shouldBe` "Hello, Haskell!"
    describe "olá" $ do
        it "retorna a string `Olá, Haskell!" $
          hello `shouldBe` "Olá, Haskell!"
    describe "soma" $ do
        it "soma números positivos" $
          soma 1 2 `shouldBe` 3
        it "soma números negativos" $
          soma (-1) (-2) `shouldBe` (-3)
        it "soma números zeros" $
          soma 0 0 `shouldBe` 0
```

###### import
Para testar algum módulo, no exemplo, `#!hs Exercise`, é necessário importá-lo;
observe que três funções foram especificadas na importação, e serão o alvo dos testes.

Para usar o *framework*, você também precisa importar os módulos que o compõem, ou pelo menos algumas de suas funções.
Duas das funções importadas na primeira linha são `#!hs it` e `#!hs describe`.

###### main
A função `#!hs main` será executada quando os testes forem disparados e, como pode ser visto no final de sua definição, `#!hs main` invoca `#!hs specs`.

Por convenção, a função `#!hs specs` descreve o conjunto de testes a ser executado.
Esta função é iniciada com um `#!hs do`, que por enquanto você pode entender como criando um **bloco** em que testes individuais são especificados usando  `#!hs describe`.

###### It describes

Cada describe inicia uma bateria de teste relacionados, especificados em seu proprio bloco `#!hs do`.
A primeira parte do describe é uma string que identifica o grupo de testes, por exemplo pelo nome da função a ser testada.

Cada `#!hs it` define um teste individual.
A string na definição do `#!hs it` descreve o caso de teste específico, por exemplo, se a entrada é negativa ou inválida.

A segunda parte do `#!hs it` faz a invocação da função sendo testada, com os parâmetros para o teste.

A última parte define o resultado esperado para a invocação, por meio de um `#!hs shouldBe`. Por exemplo, o resultado da soma de 1 e 1 *should be* 2.

###### Outras funcionalidades.
HSpec permite testes muito mais complexos do que estes exemplos acima, por exemplo testando se erros foram emitidos pela função quando parâmetros inválidos são passados.

HSpec pode também ser combinado com QuickCheck, mas fazê-lo requer funcionalidades de Haskell ainda não estudadas, então deixaremos este ponto para mais adiante.


[^hspec]: https://hspec.github.io