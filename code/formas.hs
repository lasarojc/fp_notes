{- 
Defina um tipo Ponto que represente um ponto no R2 como uma dupla de números reais.

p1 :: Ponto
p1 = (10.0,11.0)
-}


{-
Defina um tipo algébrico FormaGeo que represente as formas geométricas básicas círculo, quadrado, retângulo e triângulo.
Cada forma deve ser posicionada usando um Ponto e ter suas dimensões fundamentais, por exemplo o raio, para o círculo.

f1 :: FormaGeo
f1 = Círculo p1 3

f2 :: FormaGeo
f2 = Quadrado p1 10.0

f3 :: FormaGeo
f2 = Retângulo p1 3 4
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



{-
Defina um tipo algébrico Operação com valores que representem as seguintes operações matemáticas básicas:
- Add (adição), Mult (multiplicação), Div (divisão), Dif (diferença) e Neg (negação do sinal).
- As operações tem 2 ou 1 operandos (somente a negação).
- As parâmetros são números inteiros.
Escreva uma função que receba uma operação e retorne o resultado da execução da operação.
- Os resultados são números inteiros (div é a divisão inteira)
- Use casamento de padrões.
-}

{-
Defina um tipo algébrico Expressão com valores que representem as seguintes operações matemáticas básicas:
- Add (adição), Mult (multiplicação), Div (divisão), Dif (diferença) e Neg (negação do sinal).
- As operações tem 2 ou 1 operandos (somente a negação).
- As parâmetros são números inteiros OU outra expressão.
Escreva uma função que receba uma expressão e retorne o cálculo expressão.
- Use recursão e casamento de padrões.
- Os resultados são números inteiros (div é a divisão inteira)
-}

















{- 
Defina um tipo Ponto que represente um ponto no R2 como uma dupla de números reais.

p1 :: Ponto
p1 = (10.0,11.0)
-}
type Ponto = (Float,Float)


{-
Defina um tipo algébrico FormaGeo que represente as formas geométricas básicas círculo, quadrado, retângulo e triângulo (retângulo).
Cada forma deve ser posicionada usando um Ponto e ter suas dimensões fundamentais, por exemplo o raio, para o círculo, base e altura para o triângulo.

f1 :: FormaGeo
f1 = Círculo p1 3

f2 :: FormaGeo
f2 = Quadrado p1 10.0

f3 :: FormaGeo
f3 = Retângulo p1 3 4

f4 :: FormaGeo
f4 = Triângulo p1 3 4
-}
data FormaGeo = Círculo Ponto Float |
                Quadrado Ponto Float |
                Retângulo Ponto Float Float |
                Triângulo Ponto Float Float


{-
Defina uma função área que tenha como parâmetro formal uma FormaGeo e retorne a área da forma correspondente.
Use Casamento de Padrões.
-}

área :: FormaGeo -> Float
área (Círculo _ r) = pi*r*r
área (Quadrado _ l) = l*l
área (Retângulo _ l1 l2)  = l1*l2
área (Triângulo _ b a) = b*a/2

{-
Defina um tipo algébrico Mão que represente os possíveis valores do jogo Pedra/Tesoura/Papel.
Defina um tipo algébrico Jogo que tenha os valores Empate e Ganha x, onde x é uma Mão.
Defina uma função que receba duas Mão como parâmetros e retorne um Jogo.
- Se uma mão for vencedora, o Jogo deve ser Ganha x, onde x é a mão ganhadora.
- Se houver empate, o Jogo deve ser Empate.
-}
data Mão = Pedra | Tesoura | Papel
data Jogo = Empate | Ganha Mão

quemGanha :: Mão -> Mão -> Jogo
quemGanha Pedra Tesoura = Ganha Pedra
quemGanha Pedra Papel = Ganha Papel
quemGanha Tesoura Pedra = Ganha Pedra
quemGanha Tesoura Papel = Ganha Tesoura
quemGanha Papel Pedra = Ganha Papel
quemGanha Papel Tesoura = Ganha Tesoura
quemGanha _ _ = Empate

{-
Defina uma função que receba uma lista de tuplas de Mão e retorne uma lista de Jogo com os respectivos resultados.
- Faça uma versão que use recursão sobre listas.
- Faça uma versão que use compreensão de listas.
-}
ganham :: [(Mão,Mão)] -> [Jogo]
ganham [] = []
ganham ((m1,m2):ms) = quemGanha m1 m2 : ganham ms

ganham' :: [(Mão,Mão)] -> [Jogo]
ganham' l = [quemGanha m1 m2 | (m1,m2) <- l]

{-
Defina uma função que receba um lista de Jogo e retorne uma lista com as Mão ganhadoras, dos jogos onde não houve empate.
- Faça uma versão que use recursão sobre listas.
- Faça uma versão que use compreensão de listas.
-}
ganhadores :: [Jogo] -> [Mão]
ganhadores [] = []
ganhadores (Empate:js) = ganhadores js
ganhadores (Ganha m : js) = m : ganhadores js

{-
Defina um tipo algébrico Op (operação) com valores que representem as seguintes operações matemáticas básicas:
- Add (adição), Mult (multiplicação), Div (divisão), Dif (diferença) e Neg (negação do sinal).
- As operações tem 2 ou 1 operandos (somente a negação).
- As parâmetros são números inteiros.
Escreva uma função que receba uma operação e retorne o resultado da execução da operação.
- Os resultados são números inteiros (div é a divisão inteira)
- Use casamento de padrões.
-}
data Op = Neg Int | Add Int Int | Mult Int Int | Div Int Int | Dif Int Int

calcula :: Op -> Int
calcula (Neg v)       = negate v
calcula (Add v1 v2)   = v1 + v2
calcula (Mult v1 v2)  = v1 * v2
calcula (Div v1 v2)   = v1 `div` v2
calcula (Dif v1 v2)   = v1 - v2



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

data Exp = ENum Int | ENeg Exp | EAdd Exp Exp | EMult Exp Exp | EDiv Exp Exp | EDif Exp Exp

calculaE :: Exp -> Int
calculaE (ENum i) = i
calculaE (ENeg e) = negate (calculaE e)
calculaE (EAdd v1 v2)   = calculaE v1 + calculaE v2
calculaE (EMult v1 v2)  = calculaE v1 * calculaE v2
calculaE (EDiv v1 v2)   = calculaE v1 `div` calculaE v2
calculaE (EDif v1 v2)   = calculaE v1 - calculaE v2
