import Test.Hspec        (Spec, it, describe, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)
import Test.QuickCheck


module Exercise where





-- Defina as seguintes funções usando ++ !! take e reverse.

{-
Defina função que retorne substring de t elementos começando na posição i
subStringDeAte "entrada1" 2 2 retorna "tr"

Entrada:
    - s: string
    - i: inteiro
    - t: inteiro
-}
subStringDeAte s i t = error "Indefinido"

{-
Defina função que retorne substring com os últimos u elementos

Entrada:
    - s: string
    - u: inteiro
-}
últimosU s u = error "Indefinido"

{-
Defina função que receba duas strings e retorne a resultado da concatenação das substrings de t elementos começando na posição i

Entrada
    - s1: string
    - s2: string
    - i: inteiro
    - u: inteiro
-}

subStringDeAteAppend s1 s2 i u = error "Indefinido"












{- 
Nos seguintes exercícios, implemente suas soluções de forma recursiva
Defina os tipos das funções.
-}


{-
Uma função que calcule x * y

Entrada:
    - x
    - y

Resultado: 
    - x * y

Exemplos:
>>>multiplique 2 3
6
>>>multiplique 3 3
9

>>>multiplique 3 (-3)
-9

>>>multiplique (-3) 3
-9

>>>multiplique (-3) (-3)
9

-}

multiplique :: Int -> Int -> Int
multiplique x n = implementar

{-
Uma função que calcule a n-ésima potência de um número x.

Entrada:
    - x: base
    - n: expoente

Resultado: 
    - x elevado a n

Exemplos:
>>>potência 2 3
8
>>>potência (-3) 2
9
-}

potência :: Int -> Int -> Int
potência x n = implementar

{-
Uma função que calcule log base 2 de n por divisões sucessivas por 2.

Entrada:
    - n

Resultado: 
    - log_2 (n)

Exemplos:
>>>logBase2 100
6

>>>logBase2 16
4

-}
logBase2 n = implementar


{-
Uma função que rotacione os elementos de uma tupla n vezes.

Entrada:
    - t: tupla de 5 inteiros.
    - n: número de rotações a ser feito. Rotacionar à direita se n é positivo e a esquerda se n é negativo.

Resultado: 
    - t rotacionado n vezes.

Exemplos:
>>>rotacionar (1,2,3,4,5) 2
(4,5,1,2,3)
>>>rotacionar (1,2,3,4,5) (-2)
(3,4,5,1,2)
-}

rotacionar t n = implementar



{-
Uma função que jogue fora os caracteres inicias de uma string s até que o restante da string se inicie com um caractere c ou que a string fique fazia.

Entrada:
    - s: string
    - c: caractere.

Resultado: 
    - a string resultante.

Exemplos:
>>>jogarForaAté "Eu quis dizer, você não quis escutar." ','
", voc\234 n\227o quis escutar."

>>>jogarForaAté "Eu quis dizer, você não quis escutar." 'z'
"zer, voc\234 n\227o quis escutar."

>>>jogarForaAté "Eu quis dizer, você não quis escutar." 'v'
"voc\234 n\227o quis escutar."

-}

jogarForaAté s c = implementar


{-
Desafio!!

A fórmula de Leibniz para pi (http://en.wikipedia.org/wiki/Leibniz_formula_for_%CF%80)
estabelece que a constante pode ser calculada como a série

pi = (4/1) - (4/3) + (4/5) - (4/7)...

Implemente uma função recursiva que calcule a constante até uma quantidade n de termos.

Entrada:
    - n: quantidade de termos

Resultado:
    - pi, calculado com n passos da série

Exemplos:

>>>piDeLeibniz 1
4.0

>>>piDeLeibniz 2
2.666666666666667

>>>piDeLeibniz 3
3.466666666666667

>>>piDeLeibniz 2000
3.1410926536210413

>>>piDeLeibniz 3000
3.1412593202657186

-}

piDeLeibniz = implementar









{- 
Nos seguintes exercícios, use listas para resolver os problemas propostos.
Em vários lugares, facilita se usar where/let-in, por exemplo se for usar a cabeca da lista múltiplas vezes

funcao l = ????
   where cabeca = head l
         cauda = tail l 
-}


{-
Um palíndromo é uma palavra que pode ser lida da esquerda para a direita ou da direita para a esquerda com o mesmo resultado, como por exemplo, ovo e Natan.
O conceito pode ser estendido para frases se ignorarmos espaços, acentos e sinais de pontuação, por exemplo, "Olé! Maracujá, caju, caramelo." mas, por enquanto, estamos interessados apenas palavras palíndromas.

Escreva uma função que teste se a entrada é uma palavra (não contém espaços).
Assuma que a entrada não tem acentos ou pontuações e que ou todas as letras são maiúsculas ou todas são minúsculas.

Entrada:
    - s - String

Resultado: 
    - True se s é uma palavra palíndroma; False caso contrário.

Exemplos:
>>>palíndromo "ana"
True
>>>palíndromo "jose"
False

>>>palíndromo "natan foi viajar"
False

>>>palíndromo "natan"
True
-}

palíndromo :: String -> Bool
palíndromo s = implementar

{-
Na próxima semana, escreva uma função que teste se uma frase é palíndroma.
-}


{-
O conceito de palíndromo pode ser aplicado a qualquer lista, por exemplo de inteiros. Neste caso, [1,2,3,4,5,4,3,2,1] seria um palíndromo.
Escreva uma função que teste se uma lista qualquer é um palíndromo.
O tipo da função está definido para você e diz que a lista pode ser de qualquer tipo que se possa comparar com um ==

Entrada:
    - s - lista de inteiros

Resultado: 
    - True se s é uma lista palíndroma; False caso contrário.

Exemplos:
>>>palindrome "ana"
True
>>>palindrome "jose"
False

>>>palindrome [1,2,3]
False

>>>palindrome [True,False,False,True]
True

-}

palindrome :: Eq a => [a] -> Bool
palindrome s = implementar


{-
Frequentemente precisamos limpar dados entrados por usuários em fomulários.
Por exemplo, precisamos tirar os espaços no início e fim dos dados digitados, como em " José de Abreu  "
Algumas linguagens tem uma função trim que remove tais espaços em branco.

Implemente uma função que elimine todos os espaços em branco no início e fim de uma string.

>>>trim "Implemente uma"
"Implemente uma"

>>>trim " Implemente uma "
"Implemente uma"

>>>trim "  Implemente uma  "
"Implemente uma"

>>>trim "   Implemente uma"
"Implemente uma"

>>>trim "Implemente uma   "
"Implemente uma"
-}
trim :: String -> String
trim s = implementar


{-
Eu não tenho uma historia bonitinha para esta função, então vamos direto ao ponto.
Escreva uma função que quebre uma String em todo lugar em que aparecer um certo caractere.

Entrada:
    - l - String a ser dividida
    - d - delimitador

Saída:
    - Lista de strings resultante da divisão.

>>>splitTodos "Por exemplo, precisamos tirar os espaços no início e fim dos dados digitados, como em José de Abreu " ' '
["Por","exemplo,","precisamos","tirar","os","espa\231os","no","in\237cio","e","fim","dos","dados","digitados,","como","em","Jos\233","de","Abreu",""]

>>>splitTodos "Por exemplo, precisamos tirar os espaços no início e fim dos dados digitados, como em José de Abreu " 'i'
["Por exemplo, prec","samos t","rar os espa\231os no ","n\237c","o e f","m dos dados d","g","tados, como em Jos\233 de Abreu "]

>>>splitTodos "Por exemplo, precisamos tirar os espaços no início e fim dos dados digitados, como em José de Abreu " 'a'
["Por exemplo, precis","mos tir","r os esp","\231os no in\237cio e fim dos d","dos digit","dos, como em Jos\233 de Abreu "]
-}
splitTodos :: [Char] -> Char -> [[Char]]
splitTodos l d = implementar




{-
Escreva uma função que retorne duplas formadas pelos por elementos das duas metades da lista, sendo o primeiro elemento do resultado formado pelo 
primeiro elemento da primeira metade da lista mais o primeiro da segunda metade da lista, o segundo elemento formado pelo segundo elemento da primeira
metade mais o segundo elemento da segunda metade e assim por diante.

>>>combinaMetades [1,2,3,4,5,6]
[(1,4),(2,5),(3,6)]

>>>combinaMetades [1,2,3,4,5,6,7]
[(1,4),(2,5),(3,6)]

-}

combinaMetades :: [a] -> [(a,a)]
combinaMetades l = implementar


{-
Escreva uma função que reverta combinaMetades. Ou seja
>>>descombinaMetades [(1,4),(2,5),(3,6)] 
[1,2,3,4,5,6]
-}
descombinaMetades :: [(a,a)] -> [a]
descombinaMetades l = implementar


{-
Escreva uma função separe repetições consecutivas dentro de uma lista.

>>>empacote "aaaabccaadeeee"
["aaaa","b","cc","aa","d","eeee"]

>>>empacote ""
[]


>>>empacote [1,1,12,2,2,3,3,3,4,4,4,3,3,3,2,2,2,1,1,1]
[[1,1],[12],[2,2],[3,3,3],[4,4,4],[3,3,3],[2,2,2],[1,1,1]]

-}

empacote :: (Eq a) => [a] -> [[a]]
empacote l = implementar

{-
Dado uma lista empacotada, como a gerada pela função anterior, gere uma lista de duplas tal que:
- para cada pacote haja uma dupla no resultado.
- a dupla tem como primeiro elemento o dado repetido na lista correspondente e como segundo elemento o comprimento de tal lista.

>>>compacte [[1,1],[12],[2,2],[3,3,3],[4,4,4],[3,3,3],[2,2,2],[1,1,1]]
[(1,2),(12,1),(2,2),(3,3),(4,3),(3,3),(2,3),(1,3)]

>>>compacte ["aaaa","b","cc","aa","d","eeee"]
[('a',4),('b',1),('c',2),('a',2),('d',1),('e',4)]

>>>compacte []
[]

>>>compacte [[1,1],[12],[2,2],[3,3,3],[4,4,4],[3,3,3]]
[(1,2),(12,1),(2,2),(3,3),(4,3),(3,3)]

-}
compacte :: [[a]] -> [(a, Int)]
compacte l = implementar


{-
Escreva uma função que reverta a função compacte, definida acima, ou seja, tal que
>>>descompacte (compacte [[1,1],[12],[2,2],[3,3,3],[4,4,4],[3,3,3]]) == [[1,1],[12],[2,2],[3,3,3],[4,4,4],[3,3,3]]
True

-}

descompacte :: [(a, Int)] -> [[a]]
descompacte l = implementar


{-
Escreva uma função que reverta a função empacote, acima, definida acima, ou seja, tal que
>>>desempacote (empacote "aaaabccaadeeee") == "aaaabccaadeeee"
True

-}

desempacote :: [[a]] -> [a]
desempacote = implementar





--Eu
nome = "teste"   --coloque seu nome aqui, como uma String
-- juro pela minha honra, que serei ético na realização desta avaliação, 
-- não consultando nada e nem ninguém, além das notas de aulas e das 
-- minha soluções para exercícios anteriores.

{-
A função maximum procura retorna o maior elemento em uma lista.
Escreva uma função com comportamento similar, recursiva.
-}

máximo :: [Int] -> Int
máximo [] = error "Erro. Não há máximo de uma lista vazia."
máximo [x] = x
máximo lista
   | x > maxTail = x
   | otherwise = maxTail
   where maxTail = máximo xs
         x = head lista
         xs = tail lista


{-
A fórmula de Bhaskara permite calcular as raízes de uma equação de segundo grau
na forma ax^2 + bx + c = 0. A resolução é normalmente dividida em duas partes, o
cálculo do discriminante, Delta, e das raízes. O Delta é calculado pela equação
seguinte:
 Delta  = b^2 - 4ac

Calculado o delta, a seguinte equação calcula as raízes.
 raiz1 = (-b + Delta^(1/2)) / 2a
 raiz2 = (-b - Delta^(1/2)) / 2a

Observe que:
 Se Delta > 0, a equação do segundo grau tem 2 raízes.
 Se Delta = 0, 1 raiz.
 Se Delta < 0, tem 0 raízes reais.


1 - Escreva uma função que receba uma tripla com os coeficientes da equação,
isto é, (a,b,c), e retorne o valor de Delta.

2 - Escreva uma função que receba uma tripla com os coeficientes da equação,
isto é, (a,b,c), retorne uma lista com as raízes da equação de segundo
grau. Defina a função usando guardas. Utilize a função delta.
-}

delta :: (Float, Float, Float) -> Float
delta (a,b,c) = b**2 - 4*a*c

raízes :: (Float,Float, Float) -> [Float]
raízes (a,b,c)
  | d > 0 = [raiz1,raiz2]
  | d == 0 = [raiz1]
  | d < 0  = []
  where d = delta (a,b,c)
        raiz1 =  (negate b + sqrt d)/(2*a)
        raiz2 =  (negate b - sqrt d)/(2*a)

{-
Considere que o preço de uma passagem de ônibus intermunicipal pode variar dependendo
da idade do passageiro
- crianças menos de 10 anos pagam 40% e bebês (abaixo de 2 anos) pagam apenas 15%. 
- pessoas com 70 anos ou mais pagam apenas 50% do preço total. 
- os demais passageiros pagam a tarifa normal, 100%. 

Faça uma função que tenha como entrada:
- o valor total da passagem,
- a data atual e 
- a data de nascimento do passageiro. 

Como saída, a função retorna o valor a ser pago. 

Obs. 1: na solução, deve ser definido o tipo data para representar a tupla de inteiros (d,m,a).
Obs. 2: assuma que as datas estão corretas.
Obs. 3: assuma que todos os meses tem 30 dias e o ano tem 360 dias.
-}

type Data = (Int, Int, Int)

valorFinal :: Float -> Data -> Data -> Float
valorFinal preço (dn,mn,an) (da, ma, aa)
   | qtdDias < doisAnos = preço * 0.15
   | qtdDias < dezAnos = preço * 0.4
   | qtdDias < setentaAnos = preço
   | otherwise = preço * 0.5
   where doisAnos = 360*2
         dezAnos = 360*10
         setentaAnos = 360 * 70
         qtdDias = (da + (ma-1)*30 + (aa-1)*360) - (dn + (mn-1)*30 + (an-1)*360)


data Filtro = Menor | Maior | Igual deriving (Eq)

{-
O tipo Filtro pode ter um dos três valores definidos na linha anterior.
Escreva uma função recursiva que receba como entrada
- tupla com Filtro f na primeira posição e inteiro i na segunda posição.
- lista de inteiros l

Retorne
- Lista com todos os inteiros em l que são menores que i, se f for Menor, maiores que i se
f for Maior, e iguais a i, se f for Igual.
-}

filtre :: (Filtro,Int) -> [Int] -> [Int]
filtre (cond,i) l
   | null l = []
   | cond == Maior && cabeça > i = cabeça: filtre (Maior,i) cauda
   | cond == Menor && cabeça < i = cabeça: filtre (Menor,i) cauda
   | cond == Igual && cabeça == i = cabeça: filtre (Igual,i) cauda
   | otherwise = filtre (cond,i) cauda
   where cabeça = head l
         cauda = tail l
         

{-
Sabendo que:
- no mercado de ações brasileiro, ações são negociadas em lotes de 100 unidades;
- cada ação é identificada por um nome único, o "ticker", por exemplo VALE3 ou BOVA11;
- quando se compra um lote de ações, ele vai para a "carteira" do comprador;
- os proprietários das ações usam o custo médio das ações para calcular lucros e prejuízos.

Implemente as seguintes funções:
* compre
  - Entrada
     + uma tupla com o ticker (String) e o preço da ação (por unidade)
     + a quantidade de ações a comprar (múltiplo do tamanho de um lote)
     + a carteira a atual, na forma de uma lista de tuplas com ticker e custo médio das ações.
  - Retorna
     + a nova carteira, corrigida pela adição das ações compradas e com preços médios atualizados.

* venda
  - Entrada
     + uma tupla com o ticker (String) e o preço da ação (por unidade)
     + a quantidade de ações a vender (múltiplo do tamanho de um lote)
     + a carteira a atual, na forma de uma lista de tuplas com ticker e custo médio das ações.
  - Retorna
     + a nova carteira, corrigida pela remoção das ações vendidas. Se a venda não for possível,
     a carteira permanece intacta.


-}

compre :: (String, Float) -> Int -> [(String, Float, Int)] -> [(String, Float, Int)]
compre (t,p) q [] = [(t, p, q)] 
compre (t,p) q ((tt,pt,qt):xs)
   | t /= tt = x: compre c q xs
   | otherwise = (tt,(pt* fromIntegral qt + p* fromIntegral q)/fromIntegral (qt+q), qt+q):xs
   where x = (tt,pt,qt)
         c = (t,p)

venda :: (String, Float) -> Int -> [(String, Float, Int)] -> [(String, Float, Int)]
venda _ _ [] = [] 
venda v@(t,_) q (x@(tt,pt,qt):xs)
   | t /= tt = x : venda v q xs
   | qt - q == 0 = xs
   | otherwise = (tt, pt, qt-q):xs











{-
Escreva uma função **recursiva**  que calcula a soma dos quadrados dos números inteiros entre os parâmetros passados, inclusive.

Entrada:
    - i - Inteiro
    - n - Inteiro
Resultado
    - i^2 + (i+1)^2 + ... + n^2

>>>somaDosQuadrados 1 3
14

>>>somaDosQuadrados 3 6
86

>>>somaDosQuadrados 5 2
54

>>>somaDosQuadrados 3 (-2)
19
-}

somaDosQuadrados i n = error "Implementar"



{-
Dado um período, escreva uma função que decida se é uma afirmação, interrogação, exclamação, ou nenhum.

Entrada:
    - periodo - String

Resultado:
    - "afirmacao", se período termina com ., ou "exclamacao" se período termina com !, ou "interrogacao" se período
    termina  com ?, ou "nada", caso contrário.
    Ignorar espaços no fim do período.

>>>tipoPeríodo "Oi."
"afirmacao"

>>>tipoPeríodo "Oi?"
"interrogacao"

>>>tipoPeríodo "Oi!"
"exclamacao"

>>>tipoPeríodo "Oi"
"nada"

>>>tipoPeríodo "Oi^"
"nada"

>>>tipoPeríodo "Oi."
"afirmacao"

>>>tipoPeríodo "Oi?     "
"interrogacao"

-}

tipoPeríodo período = error "Implementar"



{-
Defina uma função que remova as primeiras duplicatas de uma lista de inteiros.

Entrada:
    - l - lista de inteiros.

Resultado:
    - lista em que as primeiras ocorrências repetidas de qualquer valor foram removidas.

>>>removeDuplicatas [1,2,3,4,5,3,7,8,3]
[1,2,4,5,7,8,3]

-}

removeDuplicatas l = error "Implementar"


{-
Defina uma função que remova as últimas duplicatas de uma lista de inteiros.

Entrada:
    - l - lista de inteiros.

Resultado:
    - lista em que as primeiras ocorrências repetidas de qualquer valor foram removidas.

>>>removeDuplicatas2 [1,2,3,4,5,3,7,8,3]
[1,2,3,4,5,7,8]

-}

removeDuplicatas2 l = error "Implementar"



{-
Mastermind 1.

Dado uma lista com 4 de 8 possíveis cores, determinar se a lista é válida.

Entrada:
    - l - Lista de inteiro, onde cada inteiro representa uma cor.

Resultado:
    - True se não há repetições, tem tamanho 4, e os inteiros estão entre 1 e 8. 
    - False se há repetições ou tamanho diferente de 4

>>>mmVálido [1,2,3,4]
True

>>>mmVálido [1,2,3,9]
False

>>>mmVálido [1,2,3,4,5]
False

>>>mmVálido [1,2,4,4]
False

-}

mmVálido l = error "Implementar"

{-
Mastermind 2

Dado duas listas, se alguma não é válida, lançar uma exceção (use error).
Se as duas são válidas (ver exercício anterior), retornar uma tupla com a quantidade de acertos bons e ótimos da jogada.

Entrada
    - config - lista de inteiro
    - jogada - lista de inteiro

Resultado
    - tupla (o,b) onde o é um inteiro com a quantidade de cores em jogada e que aparecem na mesma posição em config
    e b é quantidade de cores em jogada e que aparecem em posições diferentes em config.


>>>tentativa [1,2,3,4] [5,6,7,8]
(0,0)

>>>tentativa [1,2,3,4] [1,2,3,4]
(4,0)

>>>tentativa [1,2,3,4] [4,3,2,1]
(0,4)

>>>tentativa [1,2,3,4] [2,1,3,4]
(2,2)

>>>tentativa [1,2,3,4,5] [2,1,3,4]
não válido
-}

tentativa config jogada = error "Implementar"








module Root.Src.Exercise where

{- 
Defina um tipo Ponto que represente um ponto no R2 como uma dupla de números reais.

p1 :: Ponto
p1 = (10.0,11.0)
-}


{-
Defina um tipo algébrico FormaGeo que represente as formas geométricas básicas círculo, quadrado, retângulo e triângulo retângulo.
Cada forma deve ser posicionada usando um Ponto e ter suas dimensões fundamentais, por exemplo o raio, para o círculo.

f1 :: FormaGeo
f1 = Círculo p1 3

f2 :: FormaGeo
f2 = Quadrado p1 10.0

f3 :: FormaGeo
f2 = Retângulo p1 3 4

f3 :: FormaGeo
f3 = Triângulo p1 b a
-}

{-
Defina uma função área que tenha como parâmetro formal uma FormaGeo e retorne a área da forma correspondente.
Use Casamento de Padrões.
-}





{-
Defina um tipo algébrico Mão que represente os possíveis valores do jogo Pedra/Tesoura/Papel.
Defina um tipo algébrico Jogo que tenha os valores Empate e Ganha x, onde x é uma Mão.
Defina uma função que receba duas Mão como parâmetros e retorne um Jogo.
- Se uma mão for vencedora, o Jogo deve ser Ganha x, onde x é a mão ganhadora.
- Se houver empate, o Jogo deve ser Empate.


Defina uma função que receba uma lista de tuplas de Mão e retorne uma lista de Jogo com os respectivos resultados.
- Faça uma versão que use recursão sobre listas.
- Faça uma versão que use compreensão de listas.

Defina uma função que receba um lista de Jogo e retorne uma lista com as Mão ganhadoras, dos jogos onde não houve empate.
- Use compreensão de listas.
-}
data Mão = Pedra | Tesoura | Papel deriving (Eq)

data Jogo = Empate | Ganha Mão

quemGanha :: Mão -> Mão -> Jogo
quemGanha Pedra Tesoura = Ganha Pedra
quemGanha Pedra Papel   = Ganha Papel
quemGanha Tesoura Pedra = Ganha Pedra
quemGanha Tesoura Papel = Ganha Tesoura
quemGanha Papel Pedra   = Ganha Papel
quemGanha Papel Tesoura = Ganha Tesoura
quemGanha _ _           = Empate

jogo' :: Mão -> Mão -> Jogo
jogo' Pedra Tesoura = Ganha Pedra
jogo' Tesoura Papel = Ganha Tesoura
jogo' Papel Pedra   = Ganha Papel
jogo' mao1 mao2 = if mao1 == mao2 
                     then Empate 
                     else Ganha mao2

quemGanham :: [(Mão,Mão)] -> [Jogo]
quemGanham []               = []
quemGanham ((m1,m2):tms)    = quemGanha m1 m2 : quemGanham tms


{-
Defina um tipo algébrico Operação com valores que representem as seguintes operações matemáticas básicas:
- Add (adição), Mult (multiplicação), Div (divisão), Dif (diferença) e Neg (negação do sinal).
- As operações tem 2 ou 1 operandos (somente a negação).
- As parâmetros são números inteiros.
Escreva uma função que receba uma operação e retorne o resultado da execução da operação.
- Os resultados são números inteiros (div é a divisão inteira)
- Use casamento de padrões.
-}

data Operação = Add | Mult | Div | Dif | Neg

{-
Defina um tipo algébrico Expressão com valores que representem as seguintes operações matemáticas básicas ou, recursivamente, outras expressões.
- Add (adição), Mult (multiplicação), Div (divisão), Dif (diferença) e Neg (negação do sinal).
- As operações tem 2 ou 1 operandos (somente a negação).
- As parâmetros são números inteiros OU outra expressão.
Escreva uma função que receba uma expressão e retorne o cálculo expressão.
- Use recursão e casamento de padrões.
- Os resultados são números inteiros (div é a divisão inteira)

Exemplo: A expressão  -1 * ((3*4) + 1) é representa assim

        EMult
       /     \
    ENeg      EAdd
   /         /     \
ENum   EMult       ENum
  |    /   \         |
  1   ENum  ENum     1
       |     |
       3     4

>>>calculaE (ENum 1)
1

>>>calculaE (ENeg (ENum 1))
-1

>>>calculaE (EMult (ENum 3) (ENum 4))
12

>>>calculaE (EMult (ENeg (ENum 1)) (EAdd (EMult (ENum 3) (ENum 4)) (ENum 1)))
-13
-}




funçãoExemploR x y = x+y

funçãoExemploL x y = x+y


{-
Resolva os seguintes exercícios, inspirados em https://www.cantab.net/users/antoni.diller/haskell/questions/quest06.pdf
usando foldr e foldl. 
Para a versão que usa foldr, o nome da função termina R; para a função que usa foldl, termina com L.


Escreva pelo menos 2 testes para cada função, no arquivo FinderSpec.hs


1 - Usando foldX, defina uma função somaQuadsX que recebe como parâmetro um inteiro n e retorna a soma dos quadrados dos primeiros n inteiros, isto é,
somaQuads n = 1^2 + 2^2 + 3^2 ... n^2

>>> somaQuadsR 3
15


2 - Usando foldX, defina uma função compX que calcule o comprimento de uma lista.

>>>compR [1..10]
10

3 - Usando foldX, defina uma função mínimoX que calcule o menor número em uma lista não vazia de inteiros.
>>> mínimoL [10,9..0]
0
>>> mínimoL [10..100]
10


4 - Usando foldX, defina uam função reverteX que reverte uma lista.

>>> reverte [1..10]
[10,9,8,7,6,5,4,3,2,1]

5 - Usando foldX, defina uma função removeDupX que recebe duas listas e que remova todo elemento da segunda lista 
que aparece na primeira lista.

>>>removeDupL [1,2,3] [0,1,2,4,3,5]
[0,4,5]

6 - Defina a função filterX usando foldX e que tenha o mesmo comportamento a função padrão filter

-}





funçãoExemploR x y = x+y

funçãoExemploL x y = x+y


{-
Resolva os seguintes exercícios sobre Entrada e Saída.
Use funções implementadas anteriormente para a parte funcional pura.

- Escreva um programa que leia a altura e peso de uma pessoa e imprima seu índice
de massa corporal e se este é alto, médio ou baixo.

- Escreva um programa que leia linha por linha e, para cada linha lida, inverta 
as palavras em cada linha. Use as funções words e unwords quebrar e reconstruir linhas
> unwords $ reverse $ words "lala lele lili"
"lili lele lala"

- Escreva uma função que entre em um loop infinito em que
  - leia um inteiro
  - coloque o inteiro, se válido, em uma árvore de busca binária
  - imprima a árvore

Escreva pelo menos 2 testes para cada função, no arquivo FinderSpec.hs
-}

pegaAltura :: IO Float
pegaAltura = do
     print "Digite sua altura:"
     altura <- getLine
     return (read altura)

