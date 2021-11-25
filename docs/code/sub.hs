{-
Precedência. 9

Parentize as seguinte expressões para que as operações sejam resolvidas de forma equivalente à resolver na ordem indicada.
- Indique resultado da expressão.
- Use o mínimo de parênteses possível.

1 + 2 * 3 ^ 4 - 5 * 6 / 7 + 8 * 9                 (3)
  7   3   8   1   4   5   2   6

1 + 2 * 3 ^ 4 - negate 5 * 6 / 7 + 8 * 9          (3)
  2   3   4   5    1     6   7   8   9

(+) 1 (*) 2 3 ^ 4 - negate 5 * 6 / 7 + (*) 8 9    (3)
 9     2      1   4   3      5   6   8  7
-}













{-
Operadores 15

Sem usar o computador, calcule o resultado das seguintes expressões, explicando a ordem de execução das operações.
- 2^2+2              (2)
- 2^(2+2)            (2)
- (2^) $ 2 + 2       (2)
- (2^) $ (+2) 2      (3)
- (2^) $ (2+) 2      (3)
- ((3-).(2+)) 2      (3)
-}

{- 
Listas 

Declare uma lista de tuplas em que os primeiros elementos são as potências de 2 e os segundos elementos indicam a ordem das potências dentro da conjunto de todas as potências de 2. A lista com os primeiros 4 elementos da lista que deve produzir são 
[(1,1),(2,2),(4,3),(8,4)], pois a primeira potência de 2 é 2^0=1, a segunda 2^1=2, e assim por diante.

- Use compreensão de listas.       (3)
- Gere uma lista infinita.         (4)

- Use recursão.                    (3)
- Limite a lista a um tamanho n.   (4)
-}









{-
Tipos definidos por usuários. 15

Um período pode ser do tipo Exclamação, se terminado em '!', Interrogação se terminado em '?' e Afirmação se terminado em '.'.

- Defina uma função que receba uma String e determine se o período é de algum dos tipos definidos acima.   (2)
- Defina um tipo de dados para representar os casos acima.                                                 (5)
- Ignore espaços no início e final da String.                                                              (2) 
- Gere um erro caso nenhum dos tipos se encaixar.                                                          (3)     
-}


{-
Type classes. 9

Defina uma função que recebe uma lista qualquer e reverte seus elementos 2 a dois. Por exemplo, se aplicada à lista
[1,2,3,4,5,6,7] a função retorna [2,1,4,3,6,5,7].          (3)

- A função deve ser aplicável a qualquer tipo de lista.    (3)
- Defina o tipo da função.                                 (3)

-}


{-
Usando foldr, reverta uma lista de um tipo qualquer.       (9)
-}

{- 
Tipos algébricos - 30

Defina um tipo correspondente a uma árvore de busca **ternária** para armazenar palavras.
Em uma árvore ternária, palavras que tem prefixos comuns compartilham um caminho correspondente na árvore, pelos filhos do meio dos nós. Palavras com prefixos **menores** são armazenadas à esquerda e palavras com prefixos **maiores** são armazenadas à direita.
Por exemplo, a árvore seguinte árvore armazena as palavras arte, areia, boa, bonita, besta, cao e dose

          b
       /  | \
     /    |   \
   a      |    \
   |      o     c
   r    / |     | \
   |   e  a     a  d 
   t   |    \   |  |
 / |   s     n  o  o
e  e   |     |     |
|      t     i     s
i      |     |     |
|      a     t     e
a            |
             a

- Instancie manualmente a árvore.
- Imprima o conteúdo da árvore usando foldr

-}