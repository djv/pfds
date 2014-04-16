module PFDS32 where

import Data.List (sort)
import Testing

data Tree a = Node {root :: a, children :: [Tree a]} deriving (Eq, Show)
type Rank = Int
type Heap a = [(Rank, Tree a)]

link :: (Ord a) => Tree a -> Tree a -> Tree a
link = error "fill in the function body"

-- | Insert an element in to a heap
--
-- >>> prop $ \x xs -> (elems $ insert x $ heap xs) == (sort $ x:xs)
insert :: (Ord a) => a -> Heap a -> Heap a
insert = error "fill in the function body"

insTree :: Ord a => (Rank, Tree a) -> Heap a -> Heap a
insTree = error "fill in the function body"

-- |
--
-- >>> prop $ \xs -> sort xs == (elems $ heap xs)
fromList :: (Ord a) => [a] -> Heap a
fromList = error "fill in the function body"

-- |
--
-- >>> prop $ \xs1 xs2 -> sort (xs1 ++ xs2) == elems (merge (heap xs1) (heap xs2))
merge :: (Ord a) => Heap a -> Heap a -> Heap a
merge = error "fill in the function body"

removeMinTree :: Ord a => Heap a -> ((Rank, Tree a), Heap a)
removeMinTree = error "fill in the function body"

-- |
--
-- >>> prop $ \(NonEmpty xs) -> (findMin $ heap xs) == (minimum xs)
findMin :: (Ord a) => Heap a -> a
findMin = error "fill in the function body"

-- |
--
-- >>> prop $ \(NonEmpty xs) -> (elems . deleteMin $ heap xs) == (tail $ sort xs)
deleteMin :: (Ord a) => Heap a -> Heap a
deleteMin = error "fill in the function body"

-- | Ex. 3.5
--
-- >>> prop $ \(NonEmpty xs) -> (findMin (heap xs)) == (findMin2 (heap xs))
findMin2 :: (Ord a) => Heap a -> a
findMin2 = error "fill in the function body"

-- Utility functions

heap :: [Int] -> Heap Int
heap = fromList

elems :: (Ord a) => Heap a -> [a]
elems ts = sort $ concatMap (elemsT . snd) ts where
  elemsT :: Tree a -> [a]
  elemsT (Node x cs) = x:(concatMap elemsT cs)
