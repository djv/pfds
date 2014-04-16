module PFDS32 where

import Data.List (sort)
import Testing

data Tree a = Node {rank :: Int, root :: a, children :: [Tree a]} deriving (Eq, Show)
type Heap a = [Tree a]

link :: (Ord a) => Tree a -> Tree a -> Tree a
link t1@(Node r x1 c1) t2@(Node _ x2 c2) =
  if x1 < x2 then Node (r + 1) x1 (t2 : c1)
             else Node (r + 1) x2 (t1 : c2)

-- | Insert an element in to a heap
--
-- >>> prop $ \x xs -> (elems $ insert x $ heap xs) == (sort $ x:xs)
insert :: (Ord a) => a -> Heap a -> Heap a
insert x h = insTree (Node 0 x []) h

insTree :: (Ord a) => Tree a -> [Tree a] -> [Tree a]
insTree t [] = [t]
insTree t ts@(t':ts') = if rank t < rank t' then t:ts
                                            else insTree (link t t') ts'

-- |
--
-- >>> prop $ \xs -> sort xs == (elems $ heap xs)
--
-- >>> prop $ \xs -> let rs = map rank $ heap xs in sort rs == rs
fromList :: (Ord a) => [a] -> Heap a
fromList xs = foldr insert [] xs

-- |
--
-- >>> prop $ \xs1 xs2 -> sort (xs1 ++ xs2) == elems (merge (heap xs1) (heap xs2))
merge :: (Ord a) => Heap a -> Heap a -> Heap a
merge h1 [] = h1
merge [] h2 = h2
merge h1@(h1':hs1) h2@(h2':hs2) = if rank h1' < rank h2'
  then h1':(merge hs1 h2)
  else if rank h1' > rank h2'
    then h2':(merge h1 hs2)
    else insTree (link h1' h2') (merge hs1 hs2)

removeMinTree :: (Ord a) => [Tree a] -> (Tree a, [Tree a])
removeMinTree [] = error "removeMinTree on an empty heap"
removeMinTree [t] = (t, [])
removeMinTree (t:ts) = if root t < root t' then (t, ts) else (t', t:ts') where
  (t', ts') = removeMinTree ts

-- |
--
-- >>> prop $ \(NonEmpty xs) -> (findMin $ heap xs) == (minimum xs)
findMin :: (Ord a) => Heap a -> a
findMin h = root . fst $ removeMinTree h

-- |
--
-- >>> prop $ \(NonEmpty xs) -> (elems . deleteMin $ heap xs) == (tail $ sort xs)
deleteMin :: (Ord a) => Heap a -> Heap a
deleteMin h = merge (reverse cs) h2 where
  (Node _ _ cs , h2) = removeMinTree h

-- | Ex. 3.5
--
-- >>> prop $ \(NonEmpty xs) -> (findMin (heap xs)) == (findMin2 (heap xs))
findMin2 :: (Ord a) => Heap a -> a
findMin2 ts = minimum $ map root ts

-- Utility functions

heap :: [Int] -> Heap Int
heap = fromList

elems :: (Ord a) => Heap a -> [a]
elems [] = []
elems ((Node _ x cs):ts) = sort $ x:(elems cs ++ elems ts)
