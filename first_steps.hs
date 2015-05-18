module Main where
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    -- Say hello to each arg
    printElements args

printElements :: [String] -> IO()
printElements [] = return ()
printElements (x:xs) = do putStrLn("Hello, " ++ x ++ "\n")
                          printElements xs
