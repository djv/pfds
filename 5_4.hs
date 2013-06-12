{-# LANGUAGE TemplateHaskell #-}

import Data.List (sort, nub)
import Test.QuickCheck
import Test.QuickCheck.All
import Test.LazySmallCheck (smallCheck)
import Control.Applicative

runTests = $quickCheckAll

data Tree a = E | T (Tree a) a (Tree a) deriving Show

insert x t = T (smaller x t) x (bigger x t)

bigger p E = E
bigger p (T a x b) = if x <= p
                       then bigger p b
                       else case a of
                              E -> T E x b
                              T a1 y a2 -> if y <= p
                                             then T (bigger p a2) x b
                                             else T (bigger p a1) y (T a2 x b)

smaller p E = E
smaller p (T a x b) = if x > p
                        then smaller p a
                        else case b of
                               E -> T a x E
                               T b1 y b2 -> if y > p
                                              then T a x (smaller p b1)
                                              else T (T a x b1) y (smaller p b2)

partition p E = (E, E)
partition p t@(T a x b) =
  if x <= p
    then case b of
           E -> (t, E)
           T b1 y b2 -> if y <= p
                          then let (small, big) = partition p b2 in
                                   (T (T a x b1) y small, big)
                          else let (small, big) = partition p b1 in
                                   (T a x small, T big y b2)
    else case a of
           E -> (E, t)
           T a1 y a2 -> if y <= p
                          then let (small, big) = partition p a2 in
                                   (T a1 y small, T big x b)
                          else let (small, big) = partition p a1 in
                                   (small, T big y (T a2 x b))

insert2 x t = T s x b where
  (s, b) = partition x t

prop_insert2 x xs = elems (insert x t) == elems (insert2 x t) where
  t = fromList (xs :: [Int])

fromList = foldr insert E

elems E = []
elems (T a x b) = elems a ++ [x] ++ elems b

prop_elems xs = sort es == es where
  es = elems $ fromList (xs :: [Int])

prop_fromList xs = sort xs == elems (fromList (xs :: [Int]))

findMin (T E x b) = x
findMin (T a x b) = findMin a

prop_findMin xs = not (null xs) ==> minimum xs == findMin (fromList (xs :: [Int]))

deleteMin (T E x b) = b
deleteMin (T (T E y a2) x b) = T a2 x b
deleteMin (T (T a1 y a2) x b) = T (deleteMin a1) y (T a2 x b)

prop_deleteMin xs = not (null xs) ==> tail (sort xs) == elems (deleteMin $
  fromList (xs :: [Int]))
