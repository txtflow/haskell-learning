module Piponi () where

f' :: Float -> (Float, String)
f' a = (a+1, "call function f'")

g' :: Float -> (Float, String)
g' a = (a+2, "call function g'")

test :: Float -> (Float, String)       
test = f' `bind` g'       

--bind :: (Float -> (Float,String)) -> (Float -> (Float, String)) -> (Float -> (Float,String)) 
--f `bind` g = \x -> let (fx,fs) = f x
--                      (gx,gs) = g fx
--                  in (gx,fs ++ gs)

bind :: (Float -> (Float,String))->((Float,String) -> (Float,String))
bind f' (gx,gs) = let (fx,fs) = f' gx
                  in (fx,gs++fs)