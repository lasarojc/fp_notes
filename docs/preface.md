# Apresentação

O paradigma de funcional é um dos muitos paradigmas de programação disponíveis e, uma vez que você tenha sido "contaminado" pelos outros, certamente não é o mais simples de se entender.
Ainda assim, é um paradigma que deve ser estudado pois seus pontos fortes tem sido incorporados cada vez mais frequentemente em *frameworks* e linguagens não necessariamente reconhecidos como funcionais. 
Por isso, neste curso estudaremos o paradigma funcional de uma forma prática, tentando sempre que possível mostrar diversas aplicações das ideias apresentadas no mundo da programação moderna.

## Agradecimentos
Antes de começarmos nosso estudo, deixo aqui o meu agradecimento aos professores da Faculdade de Computação da UFU que forneceram o material sobre o qual a primeira versão destas notas de aula foram baseadas: Profa. Gina Maira B. Oliveira, Profa. Maria Adriana Vidigal de Lima, e Prof. Henrique Fernandes.

## Convenções
Neste documento, usamos diversos recursos visuais com diferentes propósitos.

* *itálico* indica termos em outras línguas, como *framework* ou *middleware*. Alguns termos, contudo, são tão corriqueiramente usados que me escapam quando escrevendo e acabam não grafados corretamente.
* **negrito** indica a introdução de termos e conceitos importantes, como **mônada** e **função de ordem superior**.
* Apontadores indicam um sítio relacionado ao termo, por exemplo, como criar um repositório no [Github](http://github.com).
A leitura dos conteúdos apontados é sugerida ao final da aula.
* Notas de rodapé[^foot] indicam referenciais teóricos importantes, com detalhes da publicação e apontadores para onde a publicação pode ser lida. Elas serão, em algum momento, substituídas por referências formais.
* Imagens não autorais são também apontadores para a fonte e tem como texto alternativo as informações da autoria.
* Caixas alinhadas à esquerda são usadas para várias finalidades. Por exemplo, para apresentar exercícios, destacar especificações, apontar tarefas a serem executas por mim.
Os diversos usos são indicados nos ícones e cores das caixas.
    
!!!exercise "Exercício"
    Isso é um exercício!

    ???example "Resposta"
        Esta é a resposta do exercício.

!!!warning "Aviso!"
    Este material está em constante evolução.


???- info inline end "Resumo"
    * Elementos visuais

* Caixas alinhadas à direita podem ser vistas como um sumário executivo do que está sendo apresentado no texto adjacente.

[^foot]: Exemplo de nota de rodapé.

Exemplos de código são colocados tanto diretamente dentro de linhas, como em `#!hs somar x y = x + y` quando em blocos separados, se mais longos.

```hs
somar :: Int -> Int -> Int
somar x y = x + y
```

Exemplos de invocações de funções são apresentados também como blocos, em que entradas são precedidas por `#!hs >` mas não as saídas.

```hs
> somar 3 4
7
> 3 + 4
7
```

O resultado da computação de uma função é apontado por `⭆`, por exemplo, `1+1 ⭆ 2`.

## Referências
Esta é uma lista não exaustiva de referencial teórico.
Esta lista será aumentada com o ponteiro de entrada para o material.
Referências a tópicos específicos são apresentadas nas seções onde são usadas, como notas de rodapé.

* [Haskell](https://en.wikibooks.org/wiki/Haskell) - Divide o estudo da linguagem em "trilhas" de diversos níveis.
* [Learn you a Haskell for a greater good](http://learnyouahaskell.com/) - Visão geral e de alto nível da linguagem.
* [Learn Haskell Programming](https://www.tutorialspoint.com/haskell/index.htm) - Tutorial para iniciantes.
* [Haskell in Depth](https://livebook.manning.com/book/haskell-in-depth)
* [Hoogle](https://hoogle.haskell.org/) - Haskell search engine.

## TODO
Esta sessão documenta o conteúdo presente e a ser adicionado nestas notas de aula, sendo principalmente para a minha própria referência.
A ordem não reflete a ordem de apresentação.

- Organização
     - [ ] Ativar plugin bibtex e organizar referências.
     - [x] Reorganizar e agrupar tópicos.

- Tipos
     - [x] Tipagem em Haskell
          - [x] Forte - sem conversão automática
          - [x] Estática - em tempo de compilação.
          - [x] Inferência - dedução de tipos não especificados
                - `#!hs :t` aplicado a variáveis

    - [x] Tipos simples
        - [x] Números
        - [x] Strings

    - [ ] Tipos compostos
        - [x] Tuplas - Heterogêneos
        - [x] Listas - Homogêneos
            - [x] range operator [1..10]
            - [x] : cons
            - [x] ++ append
            - [x] !! get at index
            - [x] Pattern matching
            - [x] Compreensão de listas (Zermello-Frankel)
            - [x] Listas infinitas
            - [ ] ordenação
                - [x] selection-sort
                - [x] quick-sort
                - [ ] Merge-sort

    - [ ] Type classes
         - [ ] Classes comuns
            - [x] Eq
            - [x] Ord
            - [x] Enum
            - [x] Show
            - [ ] Read
            - [ ] Bounded
         - [x] deriving
         - [x] instance
         - [ ] Novas type classes - http://learnyouahaskell.com/making-our-own-types-and-typeclasses#algebraic-data-types

    - [x] Tipos especificados pelo usuário
        - [ ] Definição - `#!hs type` - apelidos para tipos
            - [x] não parametrizado
            - [ ] parametrizado - Variáveis de tipo
        - [ ] Tipos algébricos 
            * [ ] Definição - `#!hs data`
                - https://mmhaskell.com/blog/2017/12/24/haskell-data-types-in-5-steps
                - https://en.wikibooks.org/wiki/Haskell/GADT#Extending_the_language
                - [x] produto x soma
                - [x] não parametrizado
                - [x] parametrizado - Variáveis de tipo
                - [ ] Record
            * [x] Casamento de padrões
                - [x] as pattern - `@`
            * [x] Tipos recursivos
                - [x] Listas
                - [x] Árvores
    - [ ] Tipos abstratos de dados
        - [ ] Fila
        - [ ] Mapa
        - [ ] Pilha


- Funções
    - [x] Equações simples
    - [x] Guardas
    - [x] Casamento de padrões
    - [x] where
    - [x] let in
    - [x] operadores
    - [x] Protótipos
        - [x] Inferência - `#!hs :t`
        - [x] Especificação manual
    - [x] Polimorfismo
        - [x] type variables 
            - http://www.decom.ufop.br/romildo/2014-1/bcc222/practices/p06-polimorfismo.pdf
            - `#!hs head :: [a] -> a`
            - `#!hs swap :: (a,b) -> (b,a)`
    - [ ] Recursão
        - [x] simples
        - [x] em listas
        - [ ] de cauda
    - [x] Ordem superior
        - [x] fold
        - [x] filter
        - [x] map
        - [x] .
        - [x] $
    - [x] Currying

- [x] Controle de fluxo
    - [x] if-then-else
    - [x] case-of

- [ ] Avaliação preguiçosa
    - [ ] mostrar com listas infinitas
        - [ ] a função cycle
    - [ ] Where - let in
        - https://takenobu-hs.github.io/downloads/haskell_lazy_evaluation.pdf
        - https://stackoverflow.com/questions/32562614/is-something-in-the-where-clause-in-haskell-only-calculated-once


- [x] Entrada e saída:
    - [x] Show
    - [x] Read
    - [x] Monads (menção)
    - [x] IO
    - [ ] GUI (e.g., GLOSS)

- [ ] Mônadas
    - https://www.youtube.com/watch?v=t1e8gqXLbsU
    - [ ] control monad
    - [x] Maybe - https://www.youtube.com/watch?v=b9FagOVqxmI
    - [ ] Either


- [ ] Haskell em produção
    - [ ] Comunicação entre linguagens
    - [ ] Paralelismo e Distribuição:
        - [ ] actors

- [ ] Cálculo Lambda:
    - [ ] Visão geral
    - [Lambda Calculus - Computerphile](https://www.youtube.com/watch?v=eis11j_iGMs&t=0s)
    - https://crypto.stanford.edu/~blynn/lambda/
    - https://youtu.be/3VQ382QG-y4
    - [ ] transparência referencial - https://takenobu-hs.github.io/downloads/haskell_lazy_evaluation.pdf
    - [ ] idempotência

- [ ] Test
    - [x] HSpec
    - [x] quickcheck - https://www.cs.umd.edu/class/spring2019/cmsc388F/lectures/randomized-testing.html
    - [ ] quickcheck + Hspec https://hspec.github.io/quickcheck.html
    - [ ] Test com IO


- Exercícios/Provas
    * https://www.cantab.net/users/antoni.diller/haskell/questions/quest07.pdf
    * https://exercism.io
    * https://www.cse.chalmers.se/edu/year/2018/course/TDA555/exam.html
    * https://www.cse.chalmers.se/edu/year/2018/course/TDA555/ex-week2.html
    * https://haskell.mooc.fi/part1#sidenote-the-.-and-operators
    * https://cs.anu.edu.au/courses/comp1100/lectures/


- [ ] Pequenos projetos
    - [ ] jogo da forca para demonstrar IO
    - [ ] jogo da velha para demonstrar IO e um pouquinho de IA
    - [ ] torres de hanoi
    - [ ] bomberman

