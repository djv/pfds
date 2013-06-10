{-# LANGUAGE TemplateHaskell #-}

import Data.List (sort, nub)
import Test.QuickCheck.All
import Test.LazySmallCheck

runTests = $quickCheckAll

data Color = R | B deriving (Eq, Show)

data Tree a = E | T {color :: Color, left :: Tree a, root :: a,
  right :: Tree a} deriving (Eq, Show)

inv1 :: (Eq a) => Tree a -> Bool
--no red node has a red child
inv1 E = True
inv1 (T c l _ r) = inv1 l && inv1 r && (if c == R
                    then (l == E || color l /= R) && (r == E || color r /= R)
                    else True)

inv2 :: Tree a -> Bool
--every path from the root contains equal number of B nodes
inv2 t = all (== head ps) ps where
  ps = paths t
  paths E = [0]
  paths (T B l _ r) = map (+1) (paths l ++ paths r)
  paths (T R l _ r) = paths l ++ paths r

valid t = inv1 t && inv2 t

member _ E = False
member x (T _ l v r) = if x < v then member x l
                                else if x > v then member x r
                                else True

insert x t = T B a y b where
  T _ a y b = snd $ ins t
  ins E = (0, T R E x E)
  ins s@(T c l v r) = if x < v then (-1, lbalance c (ins l) v r)
                               else if x > v then (1, rbalance c l v (ins r))
                               else (0, s)

{-ex. 3.10-}
lbalance B (-1, (T R (T R a x b) y c)) z d = T R (T B a x b) y (T B c z d)
lbalance B (1, (T R a x (T R b y c))) z d = T R (T B a x b) y (T B c z d)
lbalance c (_, l) v r = T c l v r
rbalance B a x (-1, (T R (T R b y c) z d)) = T R (T B a x b) y (T B c z d)
rbalance B a x (1, (T R b y (T R c z d))) = T R (T B a x b) y (T B c z d)
rbalance c l v (_, r) = T c l v r

fromList xs = foldr insert E xs

elems E = []
elems (T _ l v r) = sort $ [v] ++ elems l ++ elems r

prop_fromList xs = (nub $ sort xs) == (elems $ fromList (xs :: [Int]))

prop_fromList_valid xs = valid $ fromList (xs :: [Int])

prop_member xs = all (\x -> member x t) xs where
  t = fromList (xs :: [Int])

prop_insert x xs = member (x :: Int) (insert x $ fromList xs)

prop_insert_valid x xs = valid $ insert x $ fromList (xs :: [Int])

other R = B
other B = R

{-ex. 3.9-}
fromOrdList xs = build (if odd d then B else R) xs where
  d = floor $ logBase 2 $ fromIntegral $ length xs
  build _ [] = E
  build c xs = T c (build (other c) xs1) x (build (other c) xs2) where
    s = length xs `div` 2
    (xs1, x:xs2) = splitAt s xs

prop_fromOrdList xs = sxs == (elems $ fromOrdList sxs) where
  sxs = nub $ sort (xs :: [Int])

prop_fromOrdList_valid xs = valid $ fromOrdList (nub $ sort xs :: [Int])

prop_member2 xs = all (\x -> member x t) xs where
  t = fromOrdList (nub $ sort xs :: [Int])
