module Piponi ( ) where

f' :: Float -> (Float, String)
f' a = (a+1, "call function f'")

g' :: Float -> (Float, String)
g' a = (a+2, "call function g'")

--test :: Float -> (Float, String)       
--test = f' `bind` g'       

bind :: (Float -> (Float,String))->((Float,String) -> (Float,String))
bind f' (gx,gs) = let (fx,fs) = f' gx
                  in (fx,gs++fs)

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


b :: [Double]
b = [1..5] ++ [5,4..(-10)] ++ [(-2)..6]

infinity :: Double
infinity = 1/0    
           
f1 [] (_,max)         = max
f1 (x:xs) (sofar,max) = let sofar' = Prelude.max (sofar + x) 0
                            max'   = Prelude.max sofar' max
                        in f1 xs (sofar',max')

f4 [] (sofar,max,i)         = (sofar,max,i)
f4 (x:xs) (sofar,max,i) = let sofar' = (sofar * x) + i
                              max'   = max + sofar'
                          in f4 xs (sofar',max',i)

x,y,z :: Num a => (a,a,a)
x = (1,0,0)
y = (0,1,0)
z = (0,0,1)

matrix f = (f x, f y, f z)

(a,b,c) .+ (d,e,f) = (a+d,b+e,c+f)
a .* (b,c,d) = (a * b,a * c,a * d)                     

apply (mx,my,mz) (sofar,max,i) = (sofar .* mx) .+ (max .* my) .+ (i .* mz)

test5 b = let m = matrix (f4 b)
              (_,max,_) = apply m (1,0,1)
          in max     
             