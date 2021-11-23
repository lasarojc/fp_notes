# Haskell

Em cada paradigma, encontramos vantagens e desvantagens.
Como desenvolvedores de tecnologia, nada mais justo que vocês conheçam todos os paradigmas e que possam, assim, escolher aquele paradigma e, dentro dele, aquela linguagem que permite resolver o seu problema da forma mais eficiente, isto é, rapidamente, com menor custo e com melhor qualidade.

Mas quanto ao paradigma funcional, em especial, é importante compreendê-lo pois muitas de suas funcionalidades tem sido incorporadas a outras linguagens, como compreensão de listas em Python, funções lambda em Java e C++, e processamento assíncrono, que pode ser visto como uma forma de avaliação preguiçosa, em diversas linguagens.

Algumas características importantes do paradigma:

* Sem efeitos colaterais: executando um programa, de repente você percebe que uma certa variável tem um valor estranho e fica se perguntando quem foi que atribuiu tal valor; efeitos colaterais são a origem de boa parte dos bugs e eliminá-los, ou ao menos reduzí-los, permite que você identifique rapidamente onde alterações poderiam ter acontecido.

* Camadas de abstração: funções podem ser compostas levando a programas verdadeiramente complexos, permitindo que abstrações sejam definidas em diversas camadas. Além disso, é possível, em teoria, ser provar a corretude de funções e, mesmo que na prática isso não se realize, há ferramentas que conseguem usar esta ideia para lhe ajudar a encontrar bugs.

* Processamento paralelo eficiente: a ausência de efeitos colaterais também implica que funções sem dependências podem ser executadas em paralelo, sem se preocupar com qualquer sincronização, isto é, sem mutexes e variáveis de condição, etc.

* Avaliação preguiçosa: funções só precisam realmente ser executadas se seus resultados são necessários e o compilador consegue atrasar a execução de funções enquanto possível.

* Recursão de calda: alguns problemas podem ser descritos muito facilmente como uma recursão que com uma iteração, mas linguagens tradicionais tem um limite no tamanho das pilhas de função; linguagens funcionais conseguem, em certas condições, usar recursão infinita, contornando esta limitação.

* Funções de ordem superior: funções podem receber outras funções como parâmetro e com isso podemos compor funções, por exemplo para implementar orientação a objetos.


###### Para cada serviço, uma ferramenta!

Como já ficou óbvio, há muitas, muitas linguagens de programação por aí e é possível escrever código bom em cada um delas, assim como é possível escrever código ruim em cada uma delas.
Também é possível escrever usando orientação a objetos ou funcionalmente em cada uma delas, mas embora seja **possível**, há aquelas linguagens que suportam melhor cada um dos paradigmas.
Dentre estas, há diversas opções de linguagens que podemos usar para estudar o paradigma funcional, por exemplo:

* Scheme
* Lisp
* ML
* F#
* Erlang
* Elixir
* Haskell
* Clojure

Neste curso, veremos princípios gerais e que, em teoria, poderiam ser aplicados em quaisquer destas linguagens.
Contudo, como a sintaxe pode variar muito de uma linguagem para outra, precisaremos nos focar em uma única.
Assim, nosso foco aqui será em Haskell, uma linguagem funcional madura e estável.


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