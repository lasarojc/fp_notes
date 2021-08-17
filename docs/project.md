Nada melhor que um bom projeto para fixar e colocar à prova o que estamos aprendendo, e nada melhor que unir o útil ao agradável na hora de escolher um projeto.

Neste semestre, o projeto que desenvolverão é um clone de um clássico dos jogos de computadores, Bomberman!
Bom, não exatamente um clone, mas os rudimentos de um clone.
Ainda assim, o projeto não é trivial e por isso vamos dividí-lo em etapas para gerenciar a complexidade.
De forma geral, podemos dividir o projeto em duas etapas:

* Etapa 1
    * Estruturas de dados para representação dos elementos do jogo usando **tuplas**
    * Funções de manipulação das estruturas
* Etapa 2
    * Definição de tipos para representação dos elementos do jogo.
    * Estruturas de dados para representação dos elementos do jogo usando **listas**
    * Funções de manipulação das estruturas
    * Visualização do tabuleiro
    * Movimentação de (pelo menos) um bomberman pelo tabuleiro usando o teclado
    

## Etapa 1
Um tabuleiro de bomberman pode ser visto como uma matriz em que cada célula é uma pilha de elementos.
Em nosso protótipo, os seguintes elementos podem estar presentes na célula:

- buraco
- grama
- presente_patins
- presente_arremesso
- bomba
- jogador_X

Algumas regras devem ser respeitadas pela pilha, onde "sobre" quer dizer imediatamente subsequente acima na pilha.

- buraco é sempre o primeiro elemento.
- um buraco só pode estar sobre outro buraco
- grama só pode estar sobre buraco
- presente só pode estar sobre grama
- pedra só pode estar sobre buraco, grama ou presente
- bomba só pode estar sobre grama
- jogador só pode estar sobre buraco, grama ou presente

Para representar a matriz do tabuleiro, usaremos uma tupla de tuplas. 
Assim, defina as seguintes estruturas de dados.
    
- Tabuleiro é uma tupla com 8 Linha.
- Linha é uma tupla com 8 Célula.
- Célula é uma pilha com 4 Item
    - Use uma tupla para representar pilha
    - Use buracos para preencher espaços na tupla
        - Por exemplo, uma célula vazia é uma tupla com 8 buracos
        - Uma célula com grama e um jogador é uma tupla com 2 buracos, uma grama e um jogador.

Para cada jogador, você precisa manter algumas informações extra como

- identificador (o X que aparece no Item jogador_X)
- localização - é uma tupla com coordenadas X e Y do tipo Int que representam a linha e coluna em o item jogador_X correspondente está.
- direção - é um caractere que indica para onde on jogador está olhando. 
    - 'N', 'S', 'L' e 'O'
- capacidades - é uma tupla com 3 elementos com um dos seguintes valores, onde a, b e c são Int
    - (Patins,a)
    - (Bomba,b)
    - (Arremesso,c)

O jogo só tem graça se tiver alguma coisa acontecendo nele.
Nesta primeira etapa, você desenvolverá as funções que permitirão criar e manipular os elementos do jogo, de acordo com algumas regras.

- Criação de um tabuleiro
- Movimentação de um jogador em qualquer dos sentidos
    - Deslocamento
    - Coleta de presente - teste de capacidade
    - Queda em buraco
    - Impossibilidade de movimento
- Arremesso
    - Teste de capacidade
    - Teste de direção
- Explosão
    - Direção
    - Capacidade
    - Eliminação de presentes
    - Eliminação de pedra
    - Eliminação de jogador
- Detecção de fim de jogo