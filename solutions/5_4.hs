module PFDS54 where

import Testing
import Data.List (sort, nub)

data Tree a = E | T (Tree a) a (Tree a) deriving Show

insert :: Ord a => a -> Tree a -> Tree a
insert x t = T (smaller x t) x (bigger x t)

bigger :: Ord a => a -> Tree a -> Tree a
bigger _ E = E
bigger p (T a x b) = if x <= p
                       then bigger p b
                       else case a of
                              E -> T E x b
                              T a1 y a2 -> if y <= p
                                             then T (bigger p a2) x b
                                             else T (bigger p a1) y (T a2 x b)

smaller :: Ord a => a -> Tree a -> Tree a
smaller _ E = E
smaller p (T a x b) = if x > p
                        then smaller p a
                        else case b of
                               E -> T a x E
                               T b1 y b2 -> if y > p
                                              then T a x (smaller p b1)
                                              else T (T a x b1) y (smaller p b2)

partition :: Ord a => a -> Tree a -> (Tree a, Tree a)
partition _ E = (E, E)
partition p t@(T a x b) =
  if x <= p
    then case b of
           E -> (t, E)
           T b1 y b2 -> if y <= p
                          then let (small, big) = partition p b2 in
                                   (T (T a x b1) y small, big)
                          else let (small, big) = partition p b1 in
                                   (T a x small, T big y b2)
    else case a of
           E -> (E, t)
           T a1 y a2 -> if y <= p
                          then let (small, big) = partition p a2 in
                                   (T a1 y small, T big x b)
                          else let (small, big) = partition p a1 in
                                   (small, T big y (T a2 x b))

-- |
--
-- >>> prop2 $ \x xs -> elems (insert x $ tree xs) == elems (insert2 x $ tree xs)
insert2 :: Ord a => a -> Tree a -> Tree a
insert2 x t = T s x b where
  (s, b) = partition x t

-- |
--
-- >>> prop2 $ \xs -> sort xs == elems (fromList (xs :: [Int]))
fromList :: Ord a => [a] -> Tree a
fromList = foldr insert E

-- |
--
-- >>> prop2 $ \xs -> let es = elems $ tree xs in sort es == es
elems :: Tree a -> [a]
elems E = []
elems (T a x b) = elems a ++ [x] ++ elems b

-- |
--
-- >>> prop2 $ \(NonEmpty xs) -> minimum xs == findMin (tree xs)
findMin :: Tree a -> a
findMin E = error "findMin on an empty tree"
findMin (T E x _) = x
findMin (T a _ _) = findMin a

-- |
--
-- >>> prop2 $ \(NonEmpty xs) -> tail (sort xs) == elems (deleteMin $ tree xs)
deleteMin :: Tree t -> Tree t
deleteMin E = error "deleteMin on an empty tree"
deleteMin (T E _ b) = b
deleteMin (T (T E _ a2) x b) = T a2 x b
deleteMin (T (T a1 y a2) x b) = T (deleteMin a1) y (T a2 x b)

-- Utility functions

tree :: [Int] -> Tree Int
tree = fromList
