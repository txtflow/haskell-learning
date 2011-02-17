fermat_t :: (Num n, Enum n) => n->n
fermat_t 0 = 0
fermat_t n = n + fermat_t (n-1)

fermat_p :: (Num n, Enum n) => n->n
fermat_p 0 = 0
fermat_p n = fermat_t(n) + fermat_p(n-1)

fac :: Num n => n->n
fac 0 = 1
fac n = n * fac(n-1)

factorialsList = map (fac) [1,2..]  

subList 0 _ = []
subList n (x:xs) = x : subList (n-1) xs




