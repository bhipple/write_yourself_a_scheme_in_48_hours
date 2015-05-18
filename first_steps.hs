module Main where
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    -- Say hello to each arg
    printElements args

    -- Print the sum of each char in the string as a number
    printSums args

printElements :: [String] -> IO()
printElements [] = return ()
printElements (x:xs) = do putStrLn("Hello, " ++ x ++ "\n")
                          printElements xs

printSums :: [String] -> IO()
printSums [] = return ()
printSums (x:xs) = do read x
                      printSums(xs)
