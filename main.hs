module Main where

import System.Environment

import Text.ParserCombinators.Parsec hiding (spaces)


-- parsing symbols
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

--parsing space
spaces :: Parser ()
spaces = skipMany1 space

--parser function
readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

--data type
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

-- read and show helpers
readS :: String -> Int
readS = read

showI :: Int -> String
showI = show


main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr (args !! 0))
