module Main where

import System.Environment(getArgs)
import Data.Maybe
import Data.List
import Data.Bits
import Data.Char

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

--exercises 1
--1
safeHead :: [a] -> Maybe a
safeHead []    = Nothing                     
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing                     
safeTail (_:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast []     = Nothing
safeLast [x]    = Just x                  
sefeLast (_:xs) = safeLast xs

safeInit :: [a] -> Maybe [a]
safeInit []     = Nothing
safeInit [_]    = Just []              
safeInit (x:xs) = Just (x:(fromMaybe [] (safeInit xs)))              
              
--2
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ []  = []
splitWith fun l = let (x,xs) = break fun l
                      in x:(splitWith fun xs)

--Data.List.transpose                         
zipList :: [[a]]->[[a]]                         
zipList []           = []
zipList ([]    :xss) = zipList xss
zipList ((x:xs):xss) = (x:[h | (h:_)<-xss]) : zipList (xs: [t | (_:t)<-xss])

--adler32

glue :: (Data.Bits.Bits t) => (t, t) -> t
glue (a,b) = (b `shiftL` 16) .|. a

base :: Int
base = 65521             
   
adler32 :: [Char] -> Int
adler32 xs = glue (foldl checkSum (1,0) xs)
    where checkSum (a,b) c =
              let a' = (a + (ord c .&. 0xff)) `mod` base
                  b' = (a' + b) `mod` base
              in (a',b')
                 


