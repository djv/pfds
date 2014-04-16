module PFDS2 where

import Testing

data Tree a = E | T (Tree a) a (Tree a) deriving (Show, Eq)

-- | Query for an element
--
-- >>> prop $ \xs -> all (`member` (tree xs)) xs
member :: Ord a => a -> Tree a -> Bool
member = error "fill in the function body"

-- | Insert an element
--
-- >>> insert (2 :: Int) E
-- T E 2 E
--
-- >>> insert 1 (insert (2 :: Int) E)
-- T (T E 1 E) 2 E
--
-- >>> prop $ \x xs -> member x (insert x (tree xs))
insert :: Ord a => a -> Tree a -> Tree a
insert = error "fill in the function body"

-- | Version of `member` with less comparisons. Ex. 2.2
--
-- >>> prop $ \xs -> all (`member2` (tree xs)) xs
--
-- >>> prop $ \x xs -> member x (tree xs) == member2 x (tree xs)
member2 :: Ord a => a -> Tree a -> Bool
member2 = error "fill in the function body"

-- | Version of `insert` which doesn't copy the 
-- whole search path. Ex. 2.3
--
-- >>> prop $ \x xs -> insert x (tree xs) == insert2 x (tree xs)
insert2 :: Ord a => a -> Tree a -> Tree a
insert2 = error "fill in the function body"

-- | Ex. 2.4
--
-- >>> prop $ \x xs -> insert x (tree xs) == insert3 x (tree xs)
insert3 :: Ord a => a -> Tree a -> Tree a
insert3 = error "fill in the function body"

-- | Builds a complete binary tree
--
-- >>> prop $ \(NonNegative d) x -> (size $ complete x (d `mod` 16)) == 2^(d `mod` 16) - 1
complete :: a -> Int -> Tree a
complete = error "fill in the function body"

-- |
--
-- >>> prop $ \(NonNegative d) x -> complete x (d `mod` 16) == complete2 x (d `mod` 16)
complete2 :: a -> Int -> Tree a
complete2 = error "fill in the function body"

-- |
--
-- >>> prop $ \(NonNegative c) -> let d = c `mod` 16 in (size (fst $ create2 1 d) == d) && (size (snd $ create2 1 d) == d + 1)
create2 :: a -> Int -> (Tree a, Tree a)
create2 = error "fill in the function body"

-- |
--
-- >>> prop $ \(NonNegative d) -> size (balanced 1 (d `mod` 16)) == d `mod` 16
balanced :: a -> Int -> Tree a
balanced = error "fill in the function body"

-- Utility functions

tree :: [Int] -> Tree Int
tree = fromList

fromList :: Ord a => [a] -> Tree a
fromList = foldr insert E

-- | Computes the number of elements in a tree
--
-- >>> prop $ \x xs -> let t = tree xs in size (insert x t) == (size t + (if member x t then 0 else 1))
size :: Tree a -> Int
size E = 0
size (T l _ r) = size l + size r + 1
