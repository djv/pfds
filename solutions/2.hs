module PFDS2 where

import Test.QuickCheck

data Tree a = E | T (Tree a) a (Tree a) deriving (Show, Eq)

-- | Query for an element
--
-- prop> all (`member` (tree xs)) xs
member :: Ord a => a -> Tree a -> Bool
member _ E = False
member x (T l v r) | x < v = member x l
                   | x > v = member x r
                   | otherwise = True
-- | Insert an element
--
-- >>> insert 2 E
-- T E 2 E
--
-- >>> insert 1 (insert 2 E)
-- T (T E 1 E) 2 E
--
-- prop> member x (insert x (tree xs))
insert :: Ord a => a -> Tree a -> Tree a
insert x E = T E x E
insert x t@(T l v r) | x < v = T (insert x l) v r
                     | x > v = T l v (insert x r)
                     | otherwise = t

-- | Version of `member` with less comparisons. Ex. 2.2
--
-- prop> all (`member2` (tree xs)) xs
--
-- prop> member x (tree xs) == member2 x (tree xs)
member2 :: Ord a => a -> Tree a -> Bool
member2 z t = member2' z t Nothing where
  member2' x E (Just v) = x == v
  member2' _ E Nothing = False
  member2' x (T l v r) m = if x < v then member2' x l m
                                    else member2' x r (Just v)
-- | Version of `insert` which doesn't copy the 
-- whole search path. Ex. 2.3
--
-- prop> insert x (tree xs) == insert2 x (tree xs)
insert2 :: Ord a => a -> Tree a -> Tree a
insert2 z t = maybe t id $ ins' z t where
  ins' :: Ord a => a -> Tree a -> Maybe (Tree a)
  ins' x E = Just (T E x E)
  ins' x (T l v r) | x < v = fmap (\l' -> T l' v r) (ins' x l)
                   | x > v = fmap (\r' -> T l v r') (ins' x r)
                   | otherwise = Nothing

-- | Ex. 2.4
--
-- prop> insert x (tree xs) == insert3 x (tree xs)
insert3 :: Ord a => a -> Tree a -> Tree a
insert3 z t = maybe t id $ ins' z t Nothing where
  ins' :: Ord a => a -> Tree a -> Maybe a -> Maybe (Tree a)
  ins' x E (Just v) = if x == v then Nothing
                                else Just (T E x E)
  ins' x E Nothing = Just (T E x E)
  ins' x (T l v r) m = if x < v then fmap (\l' -> T l' v r) (ins' x l m)
                                else fmap (\r' -> T l v r') (ins' x r (Just v))

-- | Builds a complete binary tree
--
-- prop> d >= 0 ==> (size $ complete x (d `mod` 16)) == 2^(d `mod` 16) - 1
complete :: a -> Int -> Tree a
complete _ 0 = E
complete x d = T t x t where
  t = complete x (d - 1)

-- |
--
-- prop> d >= 0 ==> complete x (d `mod` 16) == complete2 x (d `mod` 16)
complete2 :: a -> Int -> Tree a
complete2 x d = iterate (\t -> T t x t) E !! d

-- |
--
-- prop> (c >= 0) ==> let d = c `mod` 16 in (size (fst $ create2 1 d) == d) && (size (snd $ create2 1 d) == d + 1)
create2 :: a -> Int -> (Tree a, Tree a)
create2 x 0 = (E, T E x E)
create2 x m = if odd m then (T t1 x t1, T t1 x t2) else (T t1 x t2, T t2 x t2) where
  (t1, t2) = create2 x ((m - 1) `div` 2)

-- |
--
-- prop> (d >= 0) ==> size (balanced 1 (d `mod` 16)) == d `mod` 16
balanced :: a -> Int -> Tree a
balanced x m = fst $ create2 x m

-- Utility functions

tree :: [Int] -> Tree Int
tree = fromList

fromList :: Ord a => [a] -> Tree a
fromList = foldr insert E

-- | Computes the number of elements in a tree
--
-- prop> let t = tree xs in size (insert x t) == (size t + (if member x t then 0 else 1))
size :: Tree a -> Int
size E = 0
size (T l _ r) = size l + size r + 1
