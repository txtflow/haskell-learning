--2.2
fermat_t :: (Num n, Enum n) => n->n
fermat_t n = fermat_t' n 0
             where fermat_t' n c | n == 0 = c
                                 | otherwise = fermat_t' (n-1) (c+n)
                                               

fermat_p :: (Num n, Enum n) => n->n
fermat_p n = fermat_p' n 0
             where fermat_p' n c | n == 0 = c
                                 | otherwise = fermat_p' (n-1) (c+fermat_t(n))           

fermat_list :: (Num n, Enum n) => [n]                                                
fermat_list = map fermat_p [1,2..]                                                     
                                               
--fermat_p_up :: (Num n, Enum n) => n->n
fermat_p_up n = fermat_p_acc n 0
                where fermat_p_acc n c | n == 0 = c
                                       | otherwise = fermat_p_acc (n-1) (c + fermat_list !! (n-1))           
                                                     
fermat_p_f n = foldl (+) 0 (take n fermat_list)


                     
                     

--1.5
--getN :: (Num n) => n->[a]->a
--getN n []     = error ""
--getN 1 (x:xs) = x
--getN n (x:xs) = getN (n-1) xs

--1.6
--atomPosition :: (Num n, Eq a) => a->[a]->n
--atomPosition a l = atomPosition' a l 1
--                   where atomPosition' a l n | l == [] = 0 
--                                             | a == head l = n            
--                                             | otherwise = atomPosition' a (tail l) (n+1)  
