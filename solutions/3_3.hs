module PDFS33 where

import Data.List (sort, nub)
import Test.QuickCheck

data Color = R | B deriving (Eq, Show)

data Tree a = E | T {color :: Color, left :: Tree a, root :: a,
  right :: Tree a} deriving (Eq, Show)

-- No red node has a red child
inv1 :: (Eq a) => Tree a -> Bool
inv1 E = True
inv1 (T c l _ r) = inv1 l && inv1 r && (if c == R
                    then (l == E || color l /= R) && (r == E || color r /= R)
                    else True)

-- | Every path from the root contains equal number of B nodes
inv2 :: Tree a -> Bool
inv2 t = all (== head ps) ps where
  ps = paths t
  paths E = [0]
  paths (T B l _ r) = map (+1) (paths l ++ paths r)
  paths (T R l _ r) = paths l ++ paths r

valid :: Eq a => Tree a -> Bool
valid t = inv1 t && inv2 t

-- |
--
-- prop> all (`member` (tree xs)) xs
member :: Ord a => a -> Tree a -> Bool
member _ E = False
member x (T _ l v r) | x < v = member x l
                     | x > v = member x r
                     | otherwise = True

-- |
--
-- prop> member x (insert x $ tree xs)
--
-- prop> valid $ insert x $ tree xs
insert :: Ord a => a -> Tree a -> Tree a
insert x t = T B a y b where
  T _ a y b = snd $ ins t
  ins E = (0, T R E x E)
  ins s@(T c l v r) | x < v = (-1, lbalance c (ins l) v r)
                    | x > v = (1, rbalance c l v (ins r))
                    | otherwise = (0, s)

-- | Ex. 3.10
lbalance :: (Eq t, Num t) =>
                    Color -> (t, Tree a) -> a -> Tree a -> Tree a
lbalance B (-1, T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
lbalance B (1, T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
lbalance c (_, l) v r = T c l v r

rbalance :: (Eq t, Num t) =>
                    Color -> Tree a -> a -> (t, Tree a) -> Tree a
rbalance B a x (-1, T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
rbalance B a x (1, T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
rbalance c l v (_, r) = T c l v r

-- |
--
-- prop> nub (sort xs) == (elems $ tree xs)
--
-- prop> valid $ tree xs
fromList :: Ord a => [a] -> Tree a
fromList = foldr insert E

-- | Ex. 3.9
--
-- prop> let sxs = nub $ sort (xs :: [Int]) in sxs == elems (fromOrdList sxs)
--
-- prop> valid $ fromOrdList (nub $ sort xs :: [Int])
--
-- prop> let t = fromOrdList (nub $ sort xs :: [Int]) in all (`member` t) xs
fromOrdList :: [a] -> Tree a
fromOrdList ys = build (if odd d then B else R) ys where
  d = floor $ logBase 2 $ fromIntegral $ length ys
  build _ [] = E
  build c xs = T c (build (other c) xs1) x (build (other c) xs2) where
    s = length xs `div` 2
    (xs1, x:xs2) = splitAt s xs

-- Utility functions

tree :: [Int] -> Tree Int
tree = fromList

elems :: Ord a => Tree a -> [a]
elems E = []
elems (T _ l v r) = sort $ [v] ++ elems l ++ elems r

other :: Color -> Color
other R = B
other B = R
