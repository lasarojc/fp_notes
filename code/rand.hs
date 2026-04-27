module Main where

import System.Random

main :: IO ()
main = do
    numeroAleatorio <- randomRIO (1,10) :: IO Int
    print numeroAleatorio
    return ()