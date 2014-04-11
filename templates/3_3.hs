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
member = error "fill in the function body"

-- |
--
-- prop> member x (insert x $ tree xs)
--
-- prop> valid $ insert x $ tree xs
insert :: Ord a => a -> Tree a -> Tree a
insert = error "fill in the function body"

-- | Ex. 3.10
lbalance :: (Eq t, Num t) =>
                    Color -> (t, Tree a) -> a -> Tree a -> Tree a
lbalance = error "fill in the function body"

rbalance :: (Eq t, Num t) =>
                    Color -> Tree a -> a -> (t, Tree a) -> Tree a
rbalance = error "fill in the function body"

-- |
--
-- prop> nub (sort xs) == (elems $ tree xs)
--
-- prop> valid $ tree xs
fromList :: Ord a => [a] -> Tree a
fromList = error "fill in the function body"

-- | Ex. 3.9
--
-- prop> let sxs = nub $ sort (xs :: [Int]) in sxs == elems (fromOrdList sxs)
--
-- prop> valid $ fromOrdList (nub $ sort xs :: [Int])
--
-- prop> let t = fromOrdList (nub $ sort xs :: [Int]) in all (`member` t) xs
fromOrdList :: [a] -> Tree a
fromOrdList = error "fill in the function body"

-- Utility functions

tree :: [Int] -> Tree Int
tree = fromList

elems :: Ord a => Tree a -> [a]
elems E = []
elems (T _ l v r) = sort $ [v] ++ elems l ++ elems r

other :: Color -> Color
other R = B
other B = R
