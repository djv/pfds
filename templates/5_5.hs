module PFDS55 where

import Data.List (sort)
import Test.QuickCheck
import qualified Test.LazySmallCheck as SC
import Control.Applicative

data Heap a = E | T a [Heap a] deriving (Show, Eq)

-- |
--
-- prop> (not $ null xs) ==> (minimum xs) == (findMin $ heap xs)
findMin :: Heap t -> t
findMin = error "fill in the function body"

merge :: Ord a => Heap a -> Heap a -> Heap a
merge = error "fill in the function body"

insert :: Ord a => a -> Heap a -> Heap a
insert = error "fill in the function body"

-- |
--
-- prop> (sort xs) == (elems $ heap xs)
fromList :: Ord a => [a] -> Heap a
fromList = error "fill in the function body"

elems :: Ord a => Heap a -> [a]
elems = error "fill in the function body"

mergePairs :: Ord a => [Heap a] -> Heap a
mergePairs = error "fill in the function body"

-- |
--
-- prop> (not $ null xs) ==> (tail $ sort xs) == (elems $ deleteMin $ heap xs)
deleteMin :: Ord a => Heap a -> Heap a
deleteMin = error "fill in the function body"

data Tree a = E' | T' (Tree a) a (Tree a) deriving Show

-- | Ex. 5.8
toBinary :: Heap a -> Tree a
toBinary = error "fill in the function body"

-- |
--
-- prop> (elems2 $ toBinary $ heap xs) == (elems $ heap xs)
elems2 :: Ord a => Tree a -> [a]
elems2 = error "fill in the function body"

-- |
--
-- prop> (not $ null xs) ==> (minimum xs) == (findMin2 $ toBinary $ heap xs)
findMin2 :: Tree t -> t
findMin2 = error "fill in the function body"

merge2 :: Ord a => Tree a -> Tree a -> Tree a
merge2 = error "fill in the function body"

insert2 :: Ord a => a -> Tree a -> Tree a
insert2 = error "fill in the function body"

-- |
--
-- prop> (sort xs) == (elems2 $ fromList2 (xs :: [Int]))
fromList2 :: Ord a => [a] -> Tree a
fromList2 = error "fill in the function body"

-- |
--
-- prop> (not $ null xs) ==> (tail $ sort xs) == (elems2 $ deleteMin2 $ fromList2 (xs :: [Int]))
deleteMin2 :: Ord a => Tree a -> Tree a
deleteMin2 = error "fill in the function body"

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
-- prop> (elems $ evalOps (ops :: [Op Int])) == (elems2 $ evalOps2 ops)
evalOp2 :: Ord a => Op a -> Tree a -> Tree a
evalOp2 D = deleteMin2
evalOp2 (I x) = insert2 x

-- |
--
-- prop> ((evalOps ops) /= E) ==> (findMin $ evalOps (ops :: [Op Int])) == (findMin2 $ evalOps2 ops)
evalOps2 :: Ord a => [Op a] -> Tree a
evalOps2 ops = foldr evalOp2 E' ops

-- Utility functions

heap :: [Int] -> Heap Int
heap = fromList
