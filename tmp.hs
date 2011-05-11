module Tmp where

import Control.Monad.State

x1 = runState (get >>= \s -> put (s+1) >> return (s^2)) 3