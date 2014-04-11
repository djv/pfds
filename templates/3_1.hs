module PDFS31 where

import Test.QuickCheck
import Data.List (sort)

data Heap a = E | H Int (Heap a) a (Heap a) deriving (Eq, Show)

-- | Merge for rank-biased leftist heap
merge1 :: (Ord a) => Heap a -> Heap a -> Heap a
merge1 = error "fill in the function body"

-- | Merge for weight-biased leftist heap. Ex. 3.4
merge2 :: (Ord a) => Heap a -> Heap a -> Heap a
merge2 = error "fill in the function body"

merge :: (Ord a) => Heap a -> Heap a -> Heap a
merge = merge1

-- | Computes the rank of a given heap
--
-- (calcRank $ heap xs) == (rank $ heap xs)
rank :: Heap a -> Int
rank = error "fill in the function body"

-- | Insert an element in to a heap
--
-- prop> (elems $ insert x $ heap xs) == (sort $ x:xs)
insert :: (Ord a) => a -> Heap a -> Heap a
insert = error "fill in the function body"

-- |
--
-- prop> (not $ null xs) ==> (findMin $ heap xs) == (minimum xs)
findMin :: (Ord a) => Heap a -> a
findMin = error "fill in the function body"

-- |
--
-- prop> (not $ null xs) ==> checkRank . deleteMin $ heap xs
deleteMin :: (Ord a) => Heap a -> Heap a
deleteMin = error "fill in the function body"

-- |
--
-- prop> (elems $ heap xs) == sort xs
--
-- prop> checkRank $ heap xs
--
-- prop> (not $ null xs) ==> (findMin $ heap xs) == (minimum xs)
fromList :: (Ord a) => [a] -> Heap a
fromList = error "fill in the function body"

-- | Ex. 3.2
--
-- prop> (elems $ insert x $ heap xs) == (elems $ insert2 x $ heap xs)
insert2 :: (Ord a) => a -> Heap a -> Heap a
insert2 = error "fill in the function body"

-- | Ex. 3.3
--
-- prop> (elems $ heap xs) == (elems $ fromList2 xs)
--
-- prop> checkRank $ fromList2 (xs :: [Int])
--
-- prop> (not $ null (xs :: [Int])) ==> (findMin $ fromList2 xs) == (minimum xs)
fromList2 :: (Ord a) => [a] -> Heap a
fromList2 = error "fill in the function body"

-- Utility functions

checkRank :: Heap a -> Bool
checkRank E = True
checkRank (H _ l _ r) = rank l >= rank r && checkRank l && checkRank r

calcRank :: Heap a -> Int
calcRank E = 0
calcRank (H _ _ _ r) = 1 + calcRank r

singleton :: a -> Heap a
singleton x = H 1 E x E

heap :: [Int] -> Heap Int
heap = fromList

toList :: Heap a -> [a]
toList E = []
toList (H _ l v r) = toList l ++ [v] ++ toList r

elems :: (Ord a) => Heap a -> [a]
elems h = sort $ toList h
