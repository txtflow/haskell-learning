module Tmp where

import Control.Monad.State

x :: ([Char], Integer)
x = runState (get >>= \s -> put (s+1) >> return "result") 10

x2 :: State Integer Integer
x2 = do
  state <- get
  put (state+1)
  return (state+10)

result :: (Integer, Integer)
result = runState x2 10

