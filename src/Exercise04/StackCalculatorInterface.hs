module Exercise04.StackCalculatorInterface where

import Exercise01.StackCalculator
import System.IO

stackMain :: IO ()
stackMain = stackMainLow []

stackMainLow :: [Int] -> IO ()
stackMainLow stack = do
    input <- getLine
    if input == "exit" then
        return ()
    else do
        let newStack = readCommand input stack
        putStrLn $ show newStack
        stackMainLow newStack
