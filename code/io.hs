module Main where


somar x y = x + y


-- função pura
--lêNomePura = x <- getLine


-- função impura
lêNomeImpura :: IO String
lêNomeImpura = getLine


lêNomeImpura' :: IO String
lêNomeImpura' = do
                    putStrLn "Digite o seu nome "
                    getLine

lêNomeImpura'' :: IO String
lêNomeImpura'' = do
                    putStr "Digite o seu nome: "
                    x <- getLine
                    putStrLn "Obrigado por digitar seu nome!"
                    return x

main :: IO ()
main = do
    nome <- lêNomeImpura''
    putStr "Olá, "
    putStr nome
    putStrLn "!"