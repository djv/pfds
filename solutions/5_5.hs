module PFDS55 where

import Data.List (sort)
import Testing
import qualified Test.LazySmallCheck as SC
import Control.Applicative

data Heap a = E | T a [Heap a] deriving (Show, Eq)

-- |
--
-- >>> prop2 $ \(NonEmpty xs) -> (minimum xs) == (findMin $ heap xs)
findMin :: Heap t -> t
findMin E = error "findMin on an empty heap"
findMin (T x _) = x

merge :: Ord a => Heap a -> Heap a -> Heap a
merge E h = h
merge h E = h
merge h1@(T a as) h2@(T b bs) = if a < b
                            then T a (h2:as)
                            else T b (h1:bs)

insert :: Ord a => a -> Heap a -> Heap a
insert x h = merge (T x []) h

-- |
--
-- >>> prop2 $ \xs -> (sort xs) == (elems $ heap xs)
fromList :: Ord a => [a] -> Heap a
fromList xs = foldr insert E xs

elems :: Ord a => Heap a -> [a]
elems E = []
elems (T a hs) = sort $ a : concatMap elems hs

mergePairs :: Ord a => [Heap a] -> Heap a
mergePairs [] = E
mergePairs [h] = h
mergePairs (h1:h2:hs) = merge (merge h1 h2) (mergePairs hs)

-- |
--
-- >>> prop2 $ \(NonEmpty xs) -> (tail $ sort xs) == (elems $ deleteMin $ heap xs)
deleteMin :: Ord a => Heap a -> Heap a
deleteMin E = E
deleteMin (T _ hs) = mergePairs hs

data Tree a = E' | T' (Tree a) a (Tree a) deriving Show

-- | Ex. 5.8
toBinary :: Heap a -> Tree a
toBinary E = E'
toBinary h = convert h [] where
  convert (T x hs) rs = T' (convert2 hs) x (convert2 rs)
  convert2 [] = E'
  convert2 (h:hs) = convert h hs

-- |
--
-- >>> prop2 $ \xs -> (elems2 $ toBinary $ heap xs) == (elems $ heap xs)
elems2 :: Ord a => Tree a -> [a]
elems2 E' = []
elems2 (T' l v r) = sort $ v : (elems2 l ++ elems2 r)

-- |
--
-- >>> prop2 $ \(NonEmpty xs) -> (minimum xs) == (findMin2 $ toBinary $ heap xs)
findMin2 :: Tree t -> t
findMin2 E' = error "findMin on an empty heap"
findMin2 (T' _ v _) = v

merge2 :: Ord a => Tree a -> Tree a -> Tree a
merge2 E' t = t
merge2 t E' = t
merge2 (T' l1 v1 E') (T' l2 v2 E') =
  if v1 < v2
    then T' (T' l2 v2 l1) v1 E'
    else T' (T' l1 v1 l2) v2 E'

insert2 :: Ord a => a -> Tree a -> Tree a
insert2 x t = merge2 (T' E' x E') t

-- |
--
-- >>> prop2 $ \xs -> (sort xs) == (elems2 $ fromList2 (xs :: [Int]))
fromList2 :: Ord a => [a] -> Tree a
fromList2 xs = foldr insert2 E' xs

-- |
--
-- >>> prop2 $ \(NonEmpty xs) -> (tail $ sort xs) == (elems2 $ deleteMin2 $ fromList2 (xs :: [Int]))
deleteMin2 :: Ord a => Tree a -> Tree a
deleteMin2 E' = E'
deleteMin2 (T' ll _ E') = mergePairs2 ll where
  mergePairs2 E' = E'
  mergePairs2 t@(T' _ _ E') = t
  mergePairs2 (T' l v (T' a x b)) = merge2 (merge2 (T' l v E') (T' a x E')) (mergePairs2 b)

data Op a = D | I a deriving Show

instance Arbitrary a => Arbitrary (Op a) where
  arbitrary = oneof [return D, I <$> arbitrary]

instance (SC.Serial a) => SC.Serial (Op a) where
    series = SC.cons1 I SC.\/ SC.cons0 D

evalOp :: Ord a => Op a -> Heap a -> Heap a
evalOp D = deleteMin
evalOp (I x) = insert x

evalOps :: Ord a => [Op a] -> Heap a
evalOps ops = foldr evalOp E ops

-- |
--
-- >>> prop2 $ \xs ops -> (elems $ evalOps (ops :: [Op Int])) == (elems2 $ evalOps2 ops)
evalOp2 :: Ord a => Op a -> Tree a -> Tree a
evalOp2 D = deleteMin2
evalOp2 (I x) = insert2 x

-- |
--
-- >>> prop2 $ \xs ops -> ((evalOps ops) /= E) ==> (findMin $ evalOps (ops :: [Op Int])) == (findMin2 $ evalOps2 ops)
evalOps2 :: Ord a => [Op a] -> Tree a
evalOps2 ops = foldr evalOp2 E' ops

-- Utility functions

heap :: [Int] -> Heap Int
heap = fromList
