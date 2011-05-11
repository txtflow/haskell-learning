<<<<<<< HEAD
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
=======
module Piponi ( ) where

bind1 :: (Float -> (Float, String)) -> (Float -> (Float, String)) -> Float -> (Float, String)
bind1 f g x = let (gx,gs) = g x
                 (fx,fs) = f gx
                 in (fx, gs ++ fs)

bind2 f g = \x -> let (gx,gs) = g x;(fx,fs) = f gx;
                  in (fx, gs ++ fs)

f :: Float -> (Float,String)
f x = (x+1,"call f\n")

g :: Float -> (Float,String)
g x = (x+2,"call g\n")

compose = f `bind1` g      

compose2 = f `bind2` g      
>>>>>>> 9fc289634b6a0ce49a52c65d22fe13fc3a923123
