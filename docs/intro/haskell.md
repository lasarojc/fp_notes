# Haskell

A primeira versão da linguagem Haskell, cujo nome é uma homenagem ao matemático e lógico **Haskell B. Curry**, apareceu em 1987 do esforço para se consolidar vários avanços propostos no paradigma funcional.
A linguagem ficou famosa **pura**, de **propósito geral** e por ter características marcantes como **avaliação preguiçosa**, e tipagem **estática**, **forte** e por **inferência**, como discutiremos mais adiante.

Mais do que uma linguagem, Haskell era uma especificação, ou série de especificações, tendo tido várias implementações distintas.
A versão de 98 foi um marco da linguagem, sendo uma versão considerada estável.
Nesta época surgiu o Glasgow Haskell Compiler (GHC), que se tornou o compilador Haskell "padrão".
A versão seguinte da linguagem começou a ser especificada em 2006 e anunciada em 2009, a Haskell 2010. Dentre os principais avanços desta versão está a possibilidade de interagir com código escrito em outras linguagens, via a *foreign function interface* (FFI).

Mas chega de história e vamos colocar a mão na massa vendo alguns exemplos linguagem.

###### Exemplos simples
Para pequenos experimentos e exercícios, podemos usar o [Repl.It](https://replit.com/languages/haskell)[^repl] para ter acesso a um interpretador Haskell sem a necessidade de instalação.
Do lado direito do sítio, na área marcada em vermelho, digite `ghci` seguido de ++enter++.


![](../images/replit.png)

Agora digite as expressões a seguir.

```Haskell
1 + 1

True && True

100 / 10

(10 + 4) * 50

3 * (-2)

2 * -1
```

Estas expressões fazem uso de operadores comuns e se comportam exatamente como você esperaria, depois de ter aprendido a programar em qualquer linguagem, em qualquer paradigma.
Operadores são **açúcar sintático** para funções, a alma da programação funcional.


[^repl]: REPL é o acrônimo para Read, Evaluate, Print, Loop.


###### Exemplos um pouco menos complexos
Para exemplos um pouco mais interessantes, usamos o lado esquerdo do sítio.
Você pode começar substituindo a linha que aparece por padrão por `#!hs soma2n a b = a + b` (em <span style="color:red">vermelho</span>).
Do lado direito então digite o comando `#!hs :load main` (ou simplesmente `#!hs :l main`) (em <span style="color:green">verde</span>), que irá carregar o arquivo sendo editado à esquerda, main.hs.

![](../images/replit2.png)

Uma vez carregado o arquivo, você pode usar suas novas funções (em <span style="color:yellow">amarelo</span>).

###### Exemplos menos simples ainda
Para desenvolver e usar código um pouco mais complexo, você deverá instalar o compilador Haskell na sua máquina, seguindo as instruções específicas. [^ghc]
Neste caso, também recomendo o uso de uma IDE.


[^ghc]:  Instruções de como instalar o GHC são específicas para cada sistema operacional. Por isso, consulte o sítio https://www.haskell.org/platform/ para instalar o Haskell na sua máquina. Frequentemente a solução é usar o [ghcup](https://www.haskell.org/ghcup/).