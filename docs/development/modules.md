# Modularização
A modularização em Haskell acontece em dois níveis, módulos e pacotes.


## Módulos
No primeiro nível de modularização, o código é organizado em módulos, que contém funções e tipos associados.
Um programa em Haskell é uma coleção de módulos, dentre os quais deve haver um módulo *`#!hs Main`*, que deve conter uma função `#!hs main` e que servirá de ponto de entrada para a execução do programa.
Se o código for apenas interpretado, **não há a necessidade da presença de uma função main**.

Cada módulo corresponde a um arquivo; o módulo pode **exportar** definições e **importar** definições de outros módulos, do mesmo time ou de bibliotecas de terceiros.
Todo módulo tem a seguinte estrutura, onde o nome do módulo deve necessariamente começar com uma letra maiúscula.

```hs
module NomeDoMódulo {(lista de funções exportadas)} where

-- imports: Nome do módulo, seguido pelas funções a serem importadas.
import Data.Char (toLower)

-- Definições de tipos e funções.
```

Módulos podem ter nomes hierarquizados, como `#!hs Números.Complexos.Operações`, o que é interpretado pelo GHC como uma organização em subdiretórios. Por exemplo, veja os seguinte módulos e organização correspondente dos arquivos.

=== "Main.hs"
    ```hs
    module Main where
    
    import Database
    import Database.Transaction
    import Server
    import Server.WebUI

    -- Código
    ```

=== "Database.hs"
    ```hs
    module Database where

    import Database.InputOutput
    import Database.Cache
    import Database.Transaction

    --- Código

    ```

=== "Cache.hs"
    ```hs
    module Database.Cache where

    import Database.InputOutput

    --- código
    ```


```
/
|
+---+-- Main.hs
|   |
|   +-- Database.hs
|   |
|   +-- Server.hs
+--- Database
|     |
|     +-- InputOutput.hs
|     |
|     +-- Cache.hs
|     |
|     +-- Transaction.hs
+--- Server
      |
      +-- WebUI.hs
      |
      +-- REST.hs
```


###### Exportando funções específicas

???todo "TODO"
    exemplo
###### Importando funções específicas

???todo "TODO"
    exemplo




## Prelude
Por padrão, o módulo **prelude**[^prelude] é carregado toda vez que executa o ghci ou compila um programa, a não ser que seja explicitamente indicado em contrário.
Este módulo contém a definição dos tipos e operadores básicos vistos anteriormente, além de muitos outros, e o GHC.Num é parte do Prelude.
Uma pequena mas interessante amostra de outros tipos e funções.

| Nome | Definição |
|------|-----------|
|`min` | Menor de 2 elementos ordenáveis|
|`max` | Maior de 2 elementos ordenáveis|
|`Semigroup` | Uma classe em que vale a associatividade |
|`Monoid`| Monóide em que há um elemento identidade |
|`putChar`| Escreve um caractere na saida padrão |
|`getString`| Lê uma string da entrada padrão |

Estes exemplos servem para mostrar como o módulo mais básico do Haskell é diverso e como a sua biblioteca é mais diversa ainda.
Além do Prelude, centenas de outros módulos estão disponíveis na Web, de compiladores a geradores de gráficos 3D, de transformadas rápidas de Fourier a *message brokers*, em repositórios como o Hackage.[^hackage]
Contudo, é preciso ter cuidado com os módulos que baixa.
Caso você encontre um módulo que queira usar, de nome `X`, bastar baixá-lo e usar o `#!hs import`. Por exemplo, para trabalhar com números complexos, voce pode usar o módulo `#!hs Data.Complex` assim:

```hs
Prelude> import Data.Complex
Prelude Data.Complex> let x = 1.0 :+ 0.0
Prelude Data.Complex> x
1.0 :+ 0.0
```



## Packages
Módulos podem ser agrupados em **pacotes** e disponibilizados como bibliotecas para outros desenvolvedores.
Isto é feito para as bibliotecas padrão da linguagem mas também para contribuições de desenvolvedores independentes.

Dificilmente você escreverá algo de útil sem importar ao menos alguns módulos da biblioteca padrão e para importar os módulos de algum pacote não padrão, você precisa primeiro instalar o pacote e configurá-o para que seja acessível ao compilador ou interpretador, uma tarefa ingrata.
Considerando que há também diversas versões do compilador que usaremos, o Haskell, e que certos pacotes dependem de versões específicas do Haskell e de outros pacotes, configurações manuais se tornam muito trabalhosas.
Em vez disso, pode-se usar ferramentas como **Cabal** e **Stack**, que permitem descrever os pacotes as serem instalados e suas respectivas versões, por projeto, e estas gerenciarão as dependências automaticamente em tempo de compilação.

Há uma certa disputa entre as duas ferramentas, bem como formas de se traduzir as informações de uma ferramenta para outra, mas isto está fora do escopo deste material.
O escopo se resume a simplesmente prover um esqueleto para uso do Stack, que permite a execução de testes automatizados tanto na sua máquina quando nos servidores em que fará a submissão dos seus exercícios.

## Esqueleto de Projeto

As listas de exercícios são disponibilizadas via GithubClassroom, o que efetivamente cria um repositório para cada lista, para cada um dos alunos.
O repositório tem uma estrutura similar ao seguinte [template](https://github.com/pluxos/autograding-template-haskell-stack).


```

|
+---+-- stack.yaml
|   |
|   +-- package.yml
+--- src
|     |
|     +-- Exercise.hs
+--- test
      |
      +-- Tests.hs
```

Depois de clonar este repositório, da raiz do mesmo, digite `#!bash stack test`. 
A definição do ambiente no arquivo `stack.yml` será usada para iniciar a execução dos testes especificados em `test/Tests.hs`, que executam funções em `src/Exercise.hs`.
Use este template para criar novos projetos e seus testes; adicione novas dependências no arquivo `stack.yml`.




[^hackage]: https://hackage.haskell.org/packages/browse
[^prelude]: Prelude: https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html
[^typeconversion] https://andrew.gibiansky.com/blog/haskell/haskell-typeclasses/#_read