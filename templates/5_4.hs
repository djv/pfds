module PFDS54 where

import Data.List (sort, nub)

data Tree a = E | T (Tree a) a (Tree a) deriving Show

insert :: Ord a => a -> Tree a -> Tree a
insert = error "fill in the function body"

bigger :: Ord a => a -> Tree a -> Tree a
bigger = error "fill in the function body"

smaller :: Ord a => a -> Tree a -> Tree a
smaller = error "fill in the function body"

partition :: Ord a => a -> Tree a -> (Tree a, Tree a)
partition = error "fill in the function body"

-- |
--
-- prop> elems (insert x $ tree xs) == elems (insert2 x $ tree xs)
insert2 :: Ord a => a -> Tree a -> Tree a
insert2 = error "fill in the function body"

-- |
--
-- prop> sort xs == elems (fromList (xs :: [Int]))
fromList :: Ord a => [a] -> Tree a
fromList = error "fill in the function body"

-- |
--
-- prop> let es = elems $ tree xs in sort es == es
elems :: Tree a -> [a]
elems = error "fill in the function body"

-- |
--
-- prop> not (null xs) ==> minimum xs == findMin (tree xs)
findMin :: Tree a -> a
findMin = error "fill in the function body"

-- |
--
-- prop> not (null xs) ==> tail (sort xs) == elems (deleteMin $ tree xs)
deleteMin :: Tree t -> Tree t
deleteMin = error "fill in the function body"

-- Utility functions

tree :: [Int] -> Tree Int
tree = fromList
