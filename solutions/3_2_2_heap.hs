module PFDS32 where

import Data.List (sort)
import Testing

data Tree a = Node {root :: a, children :: [Tree a]} deriving (Eq, Show)
type Rank = Int
type Heap a = [(Rank, Tree a)]

link :: (Ord a) => Tree a -> Tree a -> Tree a
link t1@(Node x1 c1) t2@(Node x2 c2) =
  if x1 < x2 then Node x1 (t2 : c1)
             else Node x2 (t1 : c2)

-- | Insert an element in to a heap
--
-- >>> prop $ \x xs -> (elems $ insert x $ heap xs) == (sort $ x:xs)
insert :: (Ord a) => a -> Heap a -> Heap a
insert x h = insTree (0, (Node x [])) h

insTree :: Ord a => (Rank, Tree a) -> Heap a -> Heap a
insTree rt [] = [rt]
insTree rt@(r, t) ts@((r', t'):ts') = if r < r' then rt:ts
                                                else insTree (r+1, (link t t')) ts'

-- |
--
-- >>> prop $ \xs -> sort xs == (elems $ heap xs)
fromList :: (Ord a) => [a] -> Heap a
fromList xs = foldr insert [] xs

-- |
--
-- >>> prop $ \xs1 xs2 -> sort (xs1 ++ xs2) == elems (merge (heap xs1) (heap xs2))
merge :: (Ord a) => Heap a -> Heap a -> Heap a
merge h1 [] = h1
merge [] h2 = h2
merge h1@((r1, t1):ts1) h2@((r2, t2):ts2) = if r1 < r2
  then (r1, t1):(merge ts1 h2)
  else if r1 > r2
    then (r2, t2):(merge h1 ts2)
    else insTree (r1+1, (link t1 t2)) (merge ts1 ts2)

removeMinTree :: Ord a => Heap a -> ((Rank, Tree a), Heap a)
removeMinTree [] = error "removeMinTree on an empty heap"
removeMinTree [rt] = (rt, [])
removeMinTree (rt@(_, t):ts) = if root t < root t' then (rt, ts) else (rt', rt:ts') where
  (rt'@(_, t'), ts') = removeMinTree ts

-- |
--
-- >>> prop $ \(NonEmpty xs) -> (findMin $ heap xs) == (minimum xs)
findMin :: (Ord a) => Heap a -> a
findMin h = root . snd . fst $ removeMinTree h

-- |
--
-- >>> prop $ \(NonEmpty xs) -> (elems . deleteMin $ heap xs) == (tail $ sort xs)
deleteMin :: (Ord a) => Heap a -> Heap a
deleteMin h = merge (zip [r,r-1..1] (reverse cs)) h2 where
  ((r, Node _ cs), h2) = removeMinTree h

-- | Ex. 3.5
--
-- >>> prop $ \(NonEmpty xs) -> (findMin (heap xs)) == (findMin2 (heap xs))
findMin2 :: (Ord a) => Heap a -> a
findMin2 ts = minimum $ map (root . snd) ts

-- Utility functions

heap :: [Int] -> Heap Int
heap = fromList

elems :: (Ord a) => Heap a -> [a]
elems ts = sort $ concatMap (elemsT . snd) ts where
  elemsT :: Tree a -> [a]
  elemsT (Node x cs) = x:(concatMap elemsT cs)
