module Main where
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many (noneOf"\"")
                char '"'
                return $ String x

parseAtom :: Parser LispVal
parseAtom = do
             first <- letter <|> symbol
             rest <- many (letter <|> digit <|> symbol)
             let atom = first:rest

             -- If the atom matches the literal true or false strings,
             -- return those. Otherwise, return the whole atom
             return $ case atom of
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom

-- The . is a function composition operator. We create a function
-- liftM(Number(read(many1 digit)))
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

-- Applies the List constructor within the Parser monad to the result
-- of a number of space-separated expressions, then lift the value
-- out of the monad
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
        head <- endBy parseExpr spaces
        tail <- char '.' >> spaces >> parseExpr
        return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
        char '\''
        x<- parseExpr
        return $ List [Atom "quote", x]

-- Algebraic Data type that defines a set of possible values that a variable
-- of type LispVal can hold. Each alternative (|) is called a "Constructor",
-- which contains a name and a data type
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right _ -> "Found value"

main :: IO ()
main = do
    args <- getArgs
    putStrLn (readExpr (args !! 0))
