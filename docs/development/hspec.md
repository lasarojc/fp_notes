# HSpec [^hspec]
Quando desenvolvemos software, é imprescindível que criemos testes para verificar a corretude de nosso código, ou pelo menos ganhar confiança em sua corretude.
HSpec é um framework que possibilita escrever e executar testes unitários de funções;
o *framework* provê uma linguagem de domínio específico para escrever os testes também em Haskell. 
Usaremos seguinte arquivo `test/Tests.hs` como exemplo para descrever a linguagem em (muito) alto nível.

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

###### It describes
A função `#!hs main` será executada quando os testes forem disparados e, como pode ser visto no final de sua definição, `#!hs main` invoca `#!hs specs`.

`#!hs specs` é iniciada com um `#!hs do`; por enquanto você pode entender esta palavra chave como criando um bloco em que vários `#!hs describe` são especificados. Cada describe inicia uma bateria de teste relacionados, especificados em seu proprio bloco `#!hs do`.

###### Should be
Cada teste é descrito com um `#!hs it`, que diz o que está sendo testado e testa se o resultado da função é o que deveria ser (*should be*), como no último exemplo, em que se testa se `#!hs soma 0 0` retorna 0 como resultado.




[^hspec]: https://hspec.github.io