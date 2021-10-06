module Main where

import Text.Read ( readMaybe ) 


data Direção = Norte | Sul | Leste | Oeste
    deriving (Show, Eq)

data Ação = ColocarBomba | Agir | Mover Direção | NO_OP | Sair
    deriving (Show, Eq)

data Item = Presente | Jogador Int | Parede | Grama

data JogadorDados = JogadorDados Int (Int,Int) String

type Tabuleiro = [[Item]]


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
        j <- vez ids
        print j
        opção <- menu
        print opção
        if opção == Sair then return ()
                         else if j `elem` ids 
                              then 
                                let (t',js') = case opção of  ColocarBomba   -> colocarBomba t js j
                                                              Agir           -> agir t js j
                                                              Mover d        -> mover d t js j
                                                              NO_OP          -> (t,js)
                                                              Sair           -> (t,js)
                                in actionLoop t' js'
                            else actionLoop t js

-- Tenta movimentar o jogador na direcao especificada.
mover :: Direção -> Tabuleiro -> [JogadorDados] -> Int -> (Tabuleiro, [JogadorDados])
mover = error "not implemented"

-- Descobre se alguma ação é possível para o jogador e executa.
agir :: Tabuleiro -> [JogadorDados] -> Int -> (Tabuleiro, [JogadorDados])
agir = error "not implemented"

-- Verifica se é possível colocar a boma e coloca.
colocarBomba :: Tabuleiro -> [JogadorDados] -> Int -> (Tabuleiro, [JogadorDados])
colocarBomba = error "not implemented"





-- Retorna IO Int com Int igual ao id do jogador.
vez :: [Int] -> IO Int
vez js = do
        print ("Escolha o jogador: " ++ show js)
        opçãoStr <- getLine
        return (maybe (-1) id (readMaybe opçãoStr))


-- Retorna IO Ação onde Ação é a próxima ação a ser executada
menu :: IO Ação
menu = do
    putStrLn "Escolha o que fazer: Norte Sul Leste Oeste Ação colocaBomba saiR"
    opção <- getLine
    return (case opção of "N" -> Mover Norte
                          "S" -> Mover Sul
                          "L" -> Mover Leste
                          "O" -> Mover Oeste
                          "A" -> Agir
                          "B" -> ColocarBomba
                          "R" -> Sair
                          _ -> NO_OP)


