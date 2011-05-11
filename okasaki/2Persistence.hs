module Persistence where
    
{-# LANGUAGE MultiParamTypeClasses#-} 

--ex 2.1 Resulting list generated in O(n) time and represented in O(n) space
suffixes    :: [a]->[[a]]
suffixes [] = [[]]
suffixes l  = l : suffixes (tail l)

class Set s a where
    empty  :: s a
    insert :: a->s a->s a
    member :: a->s a->Bool             
              
data Tree a = Empty | Node a (Tree a) (Tree a)
            deriving(Show, Eq)

-- instance (Ord a)=>(Set Tree a) where
--     empty = Empty
--     insert a Empty = Node a Empty Empty
                           