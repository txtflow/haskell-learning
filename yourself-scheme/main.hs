module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import Text.ParserCombinators.Parsec.Prim
  
main :: IO()  
main = do
  putStrLn "What is your name?"
  name <- getLine
  putStrLn $ "Hello " ++ name

symbol :: Parser Char
symbol = oneOf "!#$&|*+-/:<=>?@^_~"

spaces = skipMany1 space
         
readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
  Left err  -> "No match: "    ++ show err
  Right val -> "Found value: " ++ show val

data LispVal = Atom String                  |
               List [LispVal]               |
               DottedList [LispVal] LispVal |
               Number Integer               |
               String String                |
               Bool Bool

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf '\"')
                 char '"'
                 return $ String x