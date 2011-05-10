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