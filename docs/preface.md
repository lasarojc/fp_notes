# Apresentação

O paradigma funcional é um dos muitos paradigmas de programação disponíveis e, uma vez que você tenha sido "contaminado" pelos outros, certamente não é o mais simples de se entender.
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


