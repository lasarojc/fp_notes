module Main where

import Text.Read ( readMaybe )
import Data.Maybe ( fromMaybe, isNothing, fromJust )


data Direção = Norte | Sul | Leste | Oeste
    deriving (Show, Eq)

data Ação = ColocarBomba | Agir | Mover Direção | NO_OP | Sair
    deriving (Show, Eq)

data Item = Presente | Jogador Int | Parede | Grama

data JogadorDados = JogadorDados Int (Int,Int) String

type Tabuleiro = [[Item]]

keyMaps = [(1,[('e',ColocarBomba),('r',Agir),('a', Mover Oeste),('s', Mover Sul),('d',Mover Leste),('w', Mover Norte),('Q', Sair)]),
           (2,[('o',ColocarBomba),('p',Agir),('j', Mover Oeste),('k', Mover Sul),('l',Mover Leste),('i', Mover Norte),('Q', Sair)])]

mapKey :: Char -> [(Int, [(Char, Ação)])] -> Maybe (Int, Ação)
mapKey c []     = Nothing
mapKey c ((j,as):jas) = case mapKey' c as of Nothing -> mapKey c jas
                                             Just a  -> Just (j,a)
    where mapKey' c [] = Nothing
          mapKey' c ((c',a):ms)
            | c == c'   = Just a
            | otherwise = mapKey' c ms

-- Retorna IO id do jogador e ação a ser executada.
pegaMov :: [Int] -> IO (Maybe (Int,Ação))
pegaMov js = do
        movChar <- getChar
        return (let mapped = mapKey movChar keyMaps
                in case mapped of Nothing     -> Nothing
                                  Just (j,a)  -> if j `elem` js then mapped
                                                                else Nothing)


main :: IO ()
main = do
    actionLoop tabuleiro jogadores
    where (tabuleiro,jogadores) = iniciarTabuleiro

tabuleiroExemplo = [[Grama,Presente,Jogador 1],[Grama],[Grama],[Grama,Presente,Jogador 2]]
jogadorDadosExemplo = [JogadorDados 1 (0,0) "BP", JogadorDados 2 (1,1) "BP" ]

iniciarTabuleiro :: (Tabuleiro,[JogadorDados])
iniciarTabuleiro = (tabuleiroExemplo, jogadorDadosExemplo)

actionLoop :: Tabuleiro -> [JogadorDados] -> IO ()
actionLoop t js =
    let ids = [i | JogadorDados i _ _ <- js] in
    do
        move <- pegaMov ids
        let (j,op) = fromMaybe (-1,NO_OP) move
        print $ "(Jogador,Ação)" ++ show (j,op)
        if op == Sair
        then return ()
        else let (t',js') = case op of
                                ColocarBomba   -> colocarBomba t js j
                                Agir           -> agir t js j
                                Mover d        -> mover d t js j
                                NO_OP          -> (t,js)
                                _              -> (t,js)
             in actionLoop t' js'

-- Tenta movimentar o jogador na direcao especificada.
mover :: Direção -> Tabuleiro -> [JogadorDados] -> Int -> (Tabuleiro, [JogadorDados])
mover d t js j = (t,js)

-- Descobre se alguma ação é possível para o jogador e executa.
agir :: Tabuleiro -> [JogadorDados] -> Int -> (Tabuleiro, [JogadorDados])
agir t js j = (t,js)

-- Verifica se é possível colocar a boma e coloca.
colocarBomba :: Tabuleiro -> [JogadorDados] -> Int -> (Tabuleiro, [JogadorDados])
colocarBomba t js i = (t,js)