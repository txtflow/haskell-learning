import Data.Monoid
import Data.Foldable
import Control.Monad.Writer
import Control.Monad.State

factl :: Integer -> Writer String Integer
factl 0 = return 1
factl n = do
  let n' = n-1
  tell $ "We've taken one way from " ++ show n ++ "\n"
  m <- factl n'
  tell $ "We've called f" ++ show m ++ "\n"
  let r = n*m
  tell $ "We've multiplied " ++ show n 
    ++ " and " ++ show m ++ "\n"
  return r