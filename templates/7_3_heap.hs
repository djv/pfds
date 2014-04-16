module PDFS73 where

import Data.List (sort)
import Testing

data Tree a = Node {root :: a, children :: [Tree a]}
  deriving (Show, Eq)
data Digit a = Zero | One (Tree a) deriving (Show, Eq)
type Schedule a = [[Digit a]]
type Heap a = ([Digit a], Schedule a)

empty :: Heap a
empty = error "fill in the function body"

link :: Ord a => Tree a -> Tree a -> Tree a
link = error "fill in the function body"

insTree :: Ord a => Tree a -> [Digit a] -> [Digit a]
insTree = error "fill in the function body"

exec :: Schedule a -> Schedule a
exec = error "fill in the function body"

-- | Insert an element in to a heap
--
-- >>> prop $ \x xs -> (elems $ insert x $ heap xs) == (sort $ x:xs)
insert :: Ord a => a -> Heap a -> Heap a
insert = error "fill in the function body"

removeMinTree :: Ord a => [Digit a] -> (Tree a, [Digit a])
removeMinTree = error "fill in the function body"

-- |
--
-- >>> prop $ \(NonEmpty xs) -> (findMin $ heap xs) == (minimum xs)
findMin :: Ord a => Heap a -> a
findMin = error "fill in the function body"

-- |
--
-- >>> prop $ \(NonEmpty xs) -> (elems . deleteMin $ heap xs) == (tail $ sort xs)
deleteMin :: Ord a => Heap a => Heap a
deleteMin = error "fill in the function body"

mrg :: Ord t => [Digit t] -> [Digit t] -> [Digit t]
mrg = error "fill in the function body"

normalize :: [Digit a] -> [Digit a]
normalize = error "fill in the function body"

elemsT :: Tree a -> [a]
elemsT = error "fill in the function body"

elemsD :: Digit a -> [a]
elemsD = error "fill in the function body"

elems :: Ord a => Heap a -> [a]
elems = error "fill in the function body"

fromList :: (Ord a) => [a] -> Heap a
fromList xs = foldr insert empty xs

-- |
--
-- >>> prop $ \xs -> sort xs == (elems $ heap xs)
heap :: [Int] -> Heap Int
heap = fromList
