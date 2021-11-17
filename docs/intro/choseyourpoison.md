# Para cada serviço, uma ferramenta!
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






[^comunicacao]: [Comunicação](https://www.dicio.com.br/comunicacao/)
[^lp]: [Linguagem de Programação](https://pt.wikipedia.org/wiki/Linguagem_de_programa%C3%A7%C3%A3o)
[^families]: [Programming Language Families](https://kremer.cpsc.ucalgary.ca/courses/seng403/W2013/papers/11ProgrammingLanguages.pdf)
[^void]: `void` é simplesmente um resultado que diz que o resultado não importa.
[^monadas]: [What is a Monad?](https://www.youtube.com/watch?v=t1e8gqXLbsU)
