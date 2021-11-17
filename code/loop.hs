module Main where

import Text.Read ( readMaybe )
import Data.Maybe ( fromMaybe, isNothing, fromJust )

-- Definição de novos tipos
data Direção = Norte | Sul | Leste | Oeste
    deriving (Show, Eq)

data Ação = ColocarBomba | Agir | Mover Direção | NO_OP | Sair
    deriving (Show, Eq)

data Item = Presente | Jogador Int | Parede | Grama

data DadosDoJogador = DadosDoJogador Int (Int,Int) String

type Tabuleiro = [[Item]]


-- Mapeamento de teclas para ações de cada um dos possíveis jogadores.
-- Cada linha corresponde a um jogador.
keyMaps = [(1,[('e',ColocarBomba),('r',Agir),('a', Mover Oeste),('s', Mover Sul),('d',Mover Leste),('w', Mover Norte),('Q', Sair)]),
           (2,[('o',ColocarBomba),('p',Agir),('j', Mover Oeste),('k', Mover Sul),('l',Mover Leste),('i', Mover Norte),('Q', Sair)])]

-- Dado uma tecla e um mapa, retorna o jogador e a ação a ser executada para ele.
-- Se a tecla é inválida, retorna Nothing.
mapKey :: Char -> [(Int, [(Char, Ação)])] -> Maybe (Int, Ação)
mapKey c []     = Nothing
mapKey c ((j,as):jas) = case mapKey' c as of Nothing -> mapKey c jas
                                             Just a  -> Just (j,a)
    where mapKey' c [] = Nothing
          mapKey' c ((c',a):ms)
            | c == c'   = Just a
            | otherwise = mapKey' c ms

-- Retorna IO id do jogador e ação a ser executada.
-- js é a lista dos jogadores ainda no jogo.
pegaMov :: [Int] -> IO (Maybe (Int,Ação))
pegaMov js = do
        movChar <- getChar
        let mapped = mapKey movChar keyMaps    -- Observe a ausência do `in` quando estamos dentro de um bloco IO.
        case mapped of Nothing     -> return Nothing
                       Just (j,a)  -> if j `elem` js then return mapped
                                                     else return Nothing


main :: IO ()
main = do
    loopPrincipal tabuleiro jogadores
    where (tabuleiro,jogadores) = iniciarTabuleiro

tabuleiroExemplo = [[Grama,Presente,Jogador 1],[Grama],[Grama],[Grama,Presente,Jogador 2]]
dadosDoJogadorExemplo = [DadosDoJogador 1 (0,0) "BP", 
                         DadosDoJogador 2 (1,1) "BP" ]

iniciarTabuleiro :: (Tabuleiro,[DadosDoJogador])
iniciarTabuleiro = (tabuleiroExemplo,      -- O tabuleiro deveria ser lido de um arquivo com a descrição de uma fase.
                    dadosDoJogadorExemplo) -- A lista de jogadores deveria ser lida de um menu que permitisse a escolha e configuração dos jogadores.

loopPrincipal :: Tabuleiro -> [DadosDoJogador] -> IO ()
loopPrincipal t js = do
    let ids = [i | DadosDoJogador i _ _ <- js]
    move <- pegaMov ids
    let (j,op) = fromMaybe (-1,NO_OP) move
    print $ "(Jogador,Ação)" ++ show (j,op)
    case op of
            ColocarBomba   -> let (t',js') = colocarBomba t js j in loopPrincipal t' js' -- poderia ser simplificado com uso da função uncurry
            Agir           -> let (t',js') = agir t js j         in loopPrincipal t' js'
            Mover d        -> let (t',js') = mover d t js j      in loopPrincipal t' js'
            NO_OP          -> loopPrincipal t js
            Sair           -> return ()


-- Tenta movimentar o jogador na direcao especificada.
mover :: Direção -> Tabuleiro -> [DadosDoJogador] -> Int -> (Tabuleiro, [DadosDoJogador])
mover d t js j = (t,js)

-- Descobre se alguma ação é possível para o jogador e executa.
agir :: Tabuleiro -> [DadosDoJogador] -> Int -> (Tabuleiro, [DadosDoJogador])
agir t js j = (t,js)

-- Verifica se é possível colocar a boma e coloca.
colocarBomba :: Tabuleiro -> [DadosDoJogador] -> Int -> (Tabuleiro, [DadosDoJogador])
colocarBomba t js i = (t,js)




--- >>> filter (\x -> x > 5) [1..23]
-- [6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23]
