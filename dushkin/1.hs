--1.1
fermat_t :: (Num n, Enum n) => n->n
fermat_t 0 = 0
fermat_t n = n + fermat_t (n-1)


--1.2
fermat_p :: (Num n, Enum n) => n->n
fermat_p 0 = 0
fermat_p n = fermat_t(n) + fermat_p(n-1)


--1.3
factorialsList = map (fac) [1,2..]  

fac :: Num n => n->n
fac 0 = 1
fac n = n * fac(n-1)

subList 0 _ = []
subList n (x:xs) = x : subList (n-1) xs

--1.4
fibonacciList = map (fibonacci) [1..]

fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = fibonacci(n-2) + fibonacci(n-1)

--1.5
getN :: (Num n) => n->[a]->a
--getN n []     = error ""
getN 1 (x:xs) = x
getN n (x:xs) = getN (n-1) xs

--1.6
atomPosition :: (Num n, Eq a) => a->[a]->n
atomPosition a l = atomPosition' a l 1
                   where atomPosition' a l n | l == [] = 0 
                                             | a == head l = n            
                                             | otherwise = atomPosition' a (tail l) (n+1)  
                         
--1.7                         
atomInsert :: Num n => n->b->[b]->[b]
atomInsert 1 e l = e : l            
atomInsert n e (l:ls) = l : atomInsert (n-1) e ls            
                         
--atomlnsert :: Num a=>b->[b]->a->[b]
--atomlnsert a l 1 = (a:l)
--atomlnsert a (x:xs) n = x : atomlnsert a xs (n-1)

--1.8
decrement :: Num n => n->n
decrement n = decrement' n 0
              where decrement' num c | (c+1) == num = c
                                     | otherwise = decrement' num (c+1)               
