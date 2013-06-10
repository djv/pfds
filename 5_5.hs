{-# LANGUAGE TemplateHaskell #-}

import Data.List (sort, nub)
import Test.QuickCheck
import Test.QuickCheck.All
import Test.LazySmallCheck
import Control.Applicative

runTests = $quickCheckAll

data Heap a = E | T a [Heap a] deriving (Show, Eq)

findMin (T x _) = x

merge E h = h
merge h E = h
merge h1@(T a as) h2@(T b bs) = if a < b
                            then T a (h2:as)
                            else T b (h1:bs)

insert x h = merge (T x []) h

fromList xs = foldr insert E xs

elems :: Ord a => Heap a -> [a]
elems E = []
elems (T a hs) = sort $ a : concatMap elems hs

prop_fromList xs = (sort xs) == (elems $ fromList (xs :: [Int]))

prop_findMin xs = (not $ null xs) ==> (minimum xs) == (findMin $ fromList (xs :: [Int]))

mergePairs [] = E
mergePairs [h] = h
mergePairs (h1:h2:hs) = merge (merge h1 h2) (mergePairs hs)

deleteMin E = E
deleteMin (T _ hs) = mergePairs hs

prop_deleteMin xs = (not $ null xs) ==> (tail $ sort xs) == (elems $ deleteMin $
  fromList (xs :: [Int]))

data Tree a = E' | T' (Tree a) a (Tree a) deriving Show

toBinary E = E'
toBinary h = convert h [] where
  convert (T x hs) rs = T' (convert2 hs) x (convert2 rs)
  convert2 [] = E'
  convert2 (h:hs) = convert h hs

elems2 E' = []
elems2 (T' l v r) = sort $ v : (elems2 l ++ elems2 r)

prop_elems2 xs = (elems2 $ toBinary h) == (elems h) where
  h = deleteMin $ fromList (xs :: [Int])

findMin2 (T' _ v _) = v

prop_findMin2 xs = (not $ null xs) ==> (minimum xs) == (findMin2 $ toBinary $
  fromList (xs :: [Int]))

merge2 E' t = t
merge2 t E' = t
merge2 t1@(T' l1 v1 E') t2@(T' l2 v2 E') =
  if v1 < v2
    then T' (T' l2 v2 l1) v1 E'
    else T' (T' l1 v1 l2) v2 E'

insert2 x t = merge2 (T' E' x E') t

fromList2 xs = foldr insert2 E' xs

prop_fromList2 xs = (sort xs) == (elems2 $ fromList2 (xs :: [Int]))

deleteMin2 E' = E'
deleteMin2 (T' l _ E') = mergePairs2 l where
  mergePairs2 E' = E'
  mergePairs2 t@(T' _ _ E') = t
  mergePairs2 (T' l v (T' a x b)) = merge2 (merge2 (T' l v E') (T' a x E')) (mergePairs2 b)

prop_deleteMin2 xs = (not $ null xs) ==> (tail $ sort xs) == (elems2 $ deleteMin2 $
  fromList2 (xs :: [Int]))

data Op a = D | I a deriving Show

instance Arbitrary a => Arbitrary (Op a) where
  arbitrary = oneof [return D, I <$> arbitrary]

instance (Serial a) => Serial (Op a) where
    series = cons1 I \/ cons0 D

evalOp :: Ord a => Op a -> Heap a -> Heap a
evalOp D = deleteMin
evalOp (I x) = insert x

evalOps ops = foldr evalOp E ops

evalOp2 :: Ord a => Op a -> Tree a -> Tree a
evalOp2 D = deleteMin2
evalOp2 (I x) = insert2 x

evalOps2 ops = foldr evalOp2 E' ops

prop_evalOp ops = (elems h) == (elems2 t) where
  h = evalOps (ops :: [Op Int])
  t = evalOps2 ops

prop_evalOp2 ops = (h /= E) ==> (findMin h) == (findMin2 t) where
  h = evalOps (ops :: [Op Int])
  t = evalOps2 ops
