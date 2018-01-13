module Exercise04.NumbersGame where

import System.Random

guess :: Int -> Int -> IO Int
guess lower upper = randomRIO (lower, upper)

numbersMain :: IO ()
numbersMain = numbersMainLow 1 100 1

-- Initial values to be used:
-- lower = 1
-- upper = 100
-- try = 1
numbersMainLow :: Int -> Int -> Int -> IO ()
numbersMainLow lower upper try = do
    number <- guess lower upper
    putStrLn ("Is it " ++ show number ++ "?")
    input <- getLine
    case input of
        "greater" -> numbersMainLow (number + 1) upper (try + 1)
        "smaller" -> numbersMainLow lower (number - 1) (try + 1)
        "yes" -> putStrLn $ "I won in " ++ show try ++ " attempts!"
