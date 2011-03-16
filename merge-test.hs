module Main123 where

import System.Environment(getArgs)

interactWith :: (String -> String) -> FilePath -> FilePath -> IO ()
interactWith fun inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (fun input)

main :: IO ()
main = mainWith mainFunction
    where mainWith fun = do
            args <- getArgs
            case args of
              [input, output] -> interactWith fun input output
              _ -> putStrLn "error: exactly two arguments needed"   
    
    
mainFunction :: t -> t
mainFunction = \x -> x

----------------------------------------------------------               
----------------------------------------------------------               

splitLines :: [Char] -> [[Char]]
splitLines [] = []
splitLines cs =
    let (pre, suf) = break isLineTerminator cs
    in pre : case suf of
               ('\r':'\n':rest) -> splitLines rest
               ('\r':rest)      -> splitLines rest
               ('\n':rest)      -> splitLines rest
               _                -> []                  


isLineTerminator :: Char -> Bool
isLineTerminator c = c == '\r' || c == '\n'    
               