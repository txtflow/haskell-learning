module Ch3 where

data List a = Cons a (List a) |
              Nil
              deriving(Show)
              

fromList        :: [a] -> List a
fromList []     = Nil 
fromList (x:xs) = Cons x (fromList xs)

toList             :: List a -> [a]
toList Nil         = []
toList (Cons x xs) = x:toList xs

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)
                     
--data Tree a = Node a (Maybe (Tree a)) (Maybe (Tree a))
--            deriving(Show)


--1, 2
length :: [a] -> Int
length list = length_acc list 0
    where
      length_acc []     acc = acc
      length_acc (_:xs) acc = length_acc xs (acc+1)           

--3

mean :: (Fractional a) => [a] -> a
mean list = (sum list) / (fromIntegral (Prelude.length list))

meanOfList :: (Integral a, Fractional a) => [a] -> a
meanOfList []   = 0 
meanOfList list = mean list 0 0
    where
      mean [] s len     = s / (fromIntegral len)
      mean (x:xs) s len = mean xs (s+x) (len+1)                      

--4
reverse :: [a]->[a]
reverse list = rev list []
    where rev [] acc     = acc
          rev (x:xs) acc = rev xs (x:acc)              

palindrome :: [a]->[a]
palindrome []   = []
palindrome list = list ++ (Ch3.reverse list)                

                           
palindrome2 :: [a]->[a]
palindrome2 []     = []
palindrome2 (x:xs) =  [x] ++ (palindrome xs) ++ [x]

--5
isPalindrome    :: (Eq a)=>[a]->Bool
isPalindrome [] = False                
isPalindrome l  = l == Ch3.reverse l

--6
qsort :: (Ord a, Eq a)=>[a]->[a]
qsort []     = []
qsort (x:xs) = qsort [l | l<-xs, l<x] ++ [x] ++
               qsort [mq | mq<-xs, mq>=x]

sort :: (a->a->Bool)->[a]->[a]
sort _   []     = []
sort (f) (x:xs) = sort f [l | l<-xs, (f x l)] ++ [x] ++
                sort f [mq | mq<-xs, not(f x mq)]

quickSort :: (Ord a) => [a] -> [a]
quickSort  = sort (>)

lengthSort :: [[a]]->[[a]]             
lengthSort = sort (\x y->(Ch3.length x)>(Ch3.length y))

--7
intersperse :: a -> [[a]] -> [a]
intersperse _ []     = []
intersperse _ [x]    = x 
intersperse s (x:xs) =  x ++ [s] ++ intersperse s xs

--8
treeHigh :: Tree->Integer
treeHigh tree = th tree 0
    where
      th Empty acc          = 0
      th (Node _ b1 b2) acc = th b1 acc+1 
             