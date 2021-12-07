# HSpec + Quickcheck[^hspec_quickcheck]

A combinação de HSpec com Quickcheck é uma ferramenta poderosa na identificação de erros.
Enquanto o primeiro automatiza os testes, o segundo multiplica a cobertura dos testes.

Para entender exatamente como estes testes funcionam, você precisa entender funções de ordem superior, que veremos mais adiante. Mas mesmo sem entender a especificação a fundo, é possível usar o seguinte código como modelo para implementar seus testes.

No caso em específico, a função soma será testada com uma centena de combinações de valores para `#hs x` e `#!hs y`.

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


[^hspec_quickcheck]: https://hspec.github.io/quickcheck.html