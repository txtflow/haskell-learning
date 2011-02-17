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


--1.5
getN :: (Num n) => n->[a]->a
--getN n []     = error ""
getN 1 (x:xs) = x
getN n (x:xs) = getN (n-1) xs





