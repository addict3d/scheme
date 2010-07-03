module Main where

import System.Environment
import Monad
import Text.ParserCombinators.Parsec hiding (spaces)


-- parsing symbols
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

--parsing space
spaces :: Parser ()
spaces = skipMany1 space

--parser function
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right _ -> "Found value"

--data type
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

--parseString
parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x


--parseAtom
parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ case atom of
                           "#t" -> Bool True
                           "#f" -> Bool False
                           otherwise -> Atom atom


--parseNumber
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit



--Expression parsing
parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber


main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr (args !! 0))
