--2.2
fermat_t :: (Num n, Enum n) => n->n
fermat_t 0 = 0
fermat_t n = fermat_t' n 0
             where fermat_t' n c | n == 0 = c
                                 | otherwise = fermat_t' (n-1) (c+n)
                                               
--1.2
fermat_p :: (Num n, Enum n) => n->n
fermat_p 0 = 0
fermat_p n = fermat_t(n) + fermat_p(n-1)

--1.5
getN :: (Num n) => n->[a]->a
getN n []     = error "empty list"
getN n (x:xs) = getN' n x xs 
                where getN' n acc (x:xs) | n == 0 = acc
                                         | otherwise = getN' (n-1) x xs
                                                     
--1.6
--atomPosition :: (Num n, Eq a) => a->[a]->n
--atomPosition a l = atomPosition' a l 1
--                   where atomPosition' a l n | l == [] = 0 
--                                             | a == head l = n            
--                                             | otherwise = atomPosition' a (tail l) (n+1)  
