module PDFS31 where

import Testing
import Data.List (sort)

data Heap a = E | H Int (Heap a) a (Heap a) deriving (Eq, Show)

-- | Merge for rank-biased leftist heap
merge1 :: (Ord a) => Heap a -> Heap a -> Heap a
merge1 E h = h
merge1 h E = h
merge1 h1@(H _ a1 x1 b1) h2@(H _ a2 x2 b2) =
  if x1 < x2 then makeT x1 a1 (merge1 b1 h2)
             else makeT x2 a2 (merge1 h1 b2)
  where
    makeT x a b = if rank a > rank b then H (rank b + 1) a x b
                                     else H (rank a + 1) b x a

-- | Merge for weight-biased leftist heap. Ex. 3.4
merge2 :: (Ord a) => Heap a -> Heap a -> Heap a
merge2 E h = h
merge2 h E = h
merge2 h1@(H r1 a1 x1 b1) h2@(H r2 a2 x2 b2) =
  if x1 < x2 then makeT (rank a1) (rank b1 + r2) x1 a1 (merge2 b1 h2)
             else makeT (rank a2) (r1 + rank b2) x2 a2 (merge2 h1 b2)
  where
    makeT ra rb x a b = if ra > rb then H (1 + rb + ra) a x b
                                   else H (1 + ra + rb) b x a

merge :: (Ord a) => Heap a -> Heap a -> Heap a
merge = merge2

calcRank :: Heap a -> Int
calcRank = calcRank2

calcRank1 :: Heap a -> Int
calcRank1 E = 0
calcRank1 (H _ _ _ r) = 1 + calcRank1 r

calcRank2 :: Heap a -> Int
calcRank2 E = 0
calcRank2 (H _ l _ r) = 1 + calcRank2 l + calcRank2 r

-- | Computes the rank of a given heap
--
-- >>> prop2 $ \xs -> (calcRank $ heap xs) == (rank $ heap xs)
rank :: Heap a -> Int
rank E = 0
rank (H r _ _ _) = r

-- | Insert an element in to a heap
--
-- >>> prop2 $ \x xs -> (elems $ insert x $ heap xs) == (sort $ x:xs)
insert :: (Ord a) => a -> Heap a -> Heap a
insert x h = merge (singleton x) h

-- |
--
-- >>> prop2 $ \(NonEmpty xs) -> (findMin $ heap xs) == (minimum xs)
findMin :: (Ord a) => Heap a -> a
findMin E = error "findMin on an empty heap"
findMin (H _ _ x _) = x

-- |
--
-- >>> prop2 $ \(NonEmpty xs) -> checkRank . deleteMin $ heap xs
deleteMin :: (Ord a) => Heap a -> Heap a
deleteMin E = error "deleteMin on an empty heap"
deleteMin (H _ a _ b) = merge a b

-- |
--
-- >>> prop2 $ \xs -> (elems $ heap xs) == sort xs
--
-- >>> prop2 $ \xs -> checkRank $ heap xs
--
-- >>> prop2 $ \(NonEmpty xs) -> (findMin $ heap xs) == (minimum xs)
fromList :: (Ord a) => [a] -> Heap a
fromList xs = foldr insert E xs

-- | Ex. 3.2
--
-- >>> prop2 $ \x xs -> (elems $ insert x $ heap xs) == (elems $ insert2 x $ heap xs)
insert2 :: (Ord a) => a -> Heap a -> Heap a
insert2 x E = singleton x
insert2 x h@(H m l v r) = if x < v then H 0 h x E
                                   else H m (insert2 x l) v r

-- | Ex. 3.3
--
-- >>> prop2 $ \xs -> (elems $ heap xs) == (elems $ fromList2 xs)
--
-- >>> prop2 $ \xs -> checkRank $ fromList2 (xs::[Int])
--
-- >>> prop2 $ \(NonEmpty xs) -> (findMin $ fromList2 (xs::[Int])) == (minimum xs)
fromList2 :: (Ord a) => [a] -> Heap a
fromList2 [] = E
fromList2 xs = head . f $ map singleton xs where
  f [] = []
  f [h] = [h]
  f (h1:h2:hs) = f $ (merge h1 h2) : (f hs)

-- Utility functions

checkRank :: Heap a -> Bool
checkRank E = True
checkRank (H _ l _ r) = rank l >= rank r && checkRank l && checkRank r

singleton :: a -> Heap a
singleton x = H 1 E x E

heap :: [Int] -> Heap Int
heap = fromList

toList :: Heap a -> [a]
toList E = []
toList (H _ l v r) = toList l ++ [v] ++ toList r

elems :: (Ord a) => Heap a -> [a]
elems h = sort $ toList h
