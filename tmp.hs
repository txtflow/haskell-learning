module Tmp where

i = 0

quux :: t -> [Char]
quux a = let a = "foo"
         in a ++ "eek!"
