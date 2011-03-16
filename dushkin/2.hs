module D2 where

--2.2
fermat_t :: (Num n, Enum n) => n->n
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
                                                     
fermat_list :: (Num n, Enum n) => [n]                                                
fermat_list = map fermat_p [1,2..]                                                     
                                               
--fermat_p_up :: (Num n, Enum n) => n->n
fermat_p_up n = fermat_p_acc n 0
                where fermat_p_acc n c | n == 0 = c
                                       | otherwise = fermat_p_acc (n-1) (c + fermat_list !! (n-1))           
                                                     
fermat_p_f n = foldl (+) 0 (take n fermat_list)

getN :: (Num n) => n->[a]->a
getN n [] = error "empty list"
getN n (x:xs)  
--getN 1 (x:xs) = x
--getN n (x:xs) = getN (n-1) xs

--1.6
--atomPosition :: (Num n, Eq a) => a->[a]->n
--atomPosition a l = atomPosition' a l 1
--                   where atomPosition' a l n | l == [] = 0 
--                                             | a == head l = n            
--                                             | otherwise = atomPosition' a (tail l) (n+1)  


--2.3 ((1,'a'),"asdf")
f23 n = (snd.fst)(n)

--2.4
isLowCase :: Char->Bool
isLowCase c = if(c >= 'a' && c < 'z') 
                then True
                else False     
                     
getLowerSymbolsCount :: Num a => [Char] -> a                    
getLowerSymbolsCount str = foldl (aggregate) 0 str
                           where aggregate s c = if(isLowCase c)
                                                   then s+1
                                                   else s     
                                                        
--2.5
minInList :: (Num a, Ord a)=>[a]->a
minInList []   = 0
minInList (x:xs) = foldl min x xs

maxInList :: (Num a, Ord a)=>[a]->a                   
maxInList []   = 0
maxInList (x:xs) = foldl max x xs

--2.6
listAnd :: [Bool]->Bool
listAnd []     = True
listAnd (x:xs) = foldr (&&) x xs
                 
listOr :: [Bool]->Bool
listOr []     = True
listOr (x:xs) = foldr (||) x xs

--2.7
multiplicity :: (Num a, Num b)=>a->b->a
multiplicity _ 0 = 0 
multiplicity 0 _ = 0 
multiplicity 1 _ = 1 
multiplicity _ 1 = 0 
multiplicity a b = a + multiplicity a (b-1) 

multiplicity_acc :: (Num n)=>n->n->n
multiplicity_acc x y = multiplicity_acc' x y 0
    where multiplicity_acc' x y a |     y == 0 = a
                                  | otherwise = multiplicity_acc' x (y-1) (a+x)

--2.8
data Quadruple a b = Quadruple a a b b

firstTwo :: Quadruple t t->[t]
firstTwo (Quadruple a b _ _) = [a,b]
                             
lastTwo :: Quadruple t t->[t]
lastTwo (Quadruple _ _ a b) = [a,b]
                                  
--2.9
data Tuple a b c d = Tuple1 a |
                     Tuple2 a b |
                     Tuple3 a b c |
                     Tuple4 a b c d

tuple1 :: Tuple a b c d -> a                            
tuple1 (Tuple1 a)       = a
tuple1 (Tuple2 a b)     = a
tuple1 (Tuple3 a b c)   = a
tuple1 (Tuple4 a b c d) = a
                            
tuple2 :: Tuple a b c d -> Maybe b                            
tuple2 (Tuple1 a)       = Nothing                            
tuple2 (Tuple2 a b)     = Just b                            
tuple2 (Tuple3 a b c)   = Just b                            
tuple2 (Tuple4 a b c d) = Just b                            
                           
--2.10
--2.11
data List a = Nil |
              Cons a (List a)

listHead (Nil)       = error "empty list"                   
listHead (Cons a _)  = a

listTail (Nil)       = error "empty list"                                          
listTail (Cons _ xs) = xs

listLength list = calculate list 0
                          where calculate Nil acc = acc
                                calculate l acc = calculate (listTail l) (acc+1)

listMap f Nil = Nil                                                  
listMap f (Cons x xs) = Cons(f x) (listMap f xs)


listFoldl f acc Nil         = acc 
listFoldl f acc (Cons x xs) = listFoldl f (f acc x) xs

listFoldr f acc Nil         = acc 
listFoldr f acc (Cons x xs) = f x (listFoldr f acc xs)

