# Biblioteca
Como em diversas outras linguagens, Haskell usa pacotes para organizar a definição de tipos e funções, colocando aquelas relacionadas no mesmo pacote.
Por padrão, o pacote **prelude**[^prelude] é carregado a não ser que seja explicitamente indicado em contrário.
Este pacote contém a definição dos tipos e operadores básicos vistos anteriormente, além de muitos outros.
Uma pequena mas interessante amostra:

| Nome | Definição |
|------|-----------|
|`min` | Menor de 2 elementos ordenáveis|
|`max` | Maior de 2 elementos ordenáveis|
|`Semigroup` | Uma classe em que vale a associatividade |
|`Monoid`| Monóide em que há um elemento identidade |
|`putChar`| Escreve um caractere na saida padrão |
|`putString`| Escreve uma string na saida padrão |
|`getChar`| Lê um caractere da entrada padrão |
|`getString`| Lê uma string da entrada padrão |

Estes exemplos servem para mostrar como o pacote mais básico do Haskell é diverso e como a sua biblioteca é diversa.
Além do Prelude, centenas de outros pacotes estão disponíveis na Web, de compiladores a geradores de gráficos 3D, de transformadas rápidas de Fourier a *message brokers*, em repositórios como o Hackage[^hackage].
Contudo, é preciso ter cuidado com os pacotes que baixa.


Com esta visita rápida ao prelude, encerramos esta introdução ao Haskell e rumamos para tópicos mais universais.
Isto é, mesmo que os tópicos vistos até agora sejam obviamente associados à programação funcional, os mesmos estão fortemente relacionados à sintaxe do Haskell.
Já nas próximas seções, veremos tópicos mais independentes, i.e., mesmo que as funções, tipos e construtos usados ainda sejam implementadas em Haskell, os conceitos por trás são mais universais.

[^hackage]: https://hackage.haskell.org/packages/browse
[^prelude]: Prelude: https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html