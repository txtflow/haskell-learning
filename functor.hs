module Functor where

data Tree a = Node a (Tree a) (Tree a) |
              Leaf deriving (Eq, Read, Show)

instance Functor Tree where
  fmap f (Node a b1 b2) = Node (f a) (fmap f b1) (fmap f b2)
  fmap _ Leaf           = Leaf