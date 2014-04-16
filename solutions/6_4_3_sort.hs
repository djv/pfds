module PDFS643 where

import Data.List
import Testing

type Sort a = (Int, [[a]])

empty :: Sort a
empty = (0, [])

-- |
--
-- >>> prop $ \xs ys -> mrg (sort (xs :: [Int])) (sort ys) == sort (xs ++ ys)
mrg :: Ord a => [a] -> [a] -> [a]
mrg [] ys = ys
mrg xs [] = xs
mrg xs@(x:xs') ys@(y:ys') =
  if x <= y then x:(mrg xs' ys)
            else y:(mrg xs ys')

-- |
--
-- >>> prop $ \x xs -> x `elem` (elems $ add x $ fromList xs)
add :: Ord a => a -> Sort a -> Sort a
add x (sz, sg) = (sz + 1, addSeg [x] sg sz) where
  addSeg :: Ord a => [a] -> [[a]] -> Int -> [[a]]
  addSeg seg segs size =
    if size `mod` 2 == 0
      then seg:segs
      else addSeg (mrg seg (head segs)) (tail segs) (size `div` 2)

-- |
--
-- >>> prop $ \xs -> sorted (fromList xs) == sort xs
sorted :: Ord a => (Int, [[a]]) -> [a]
sorted (_, segs) = foldl mrg [] segs

-- Utility functions

elems :: Sort a -> [a]
elems (_, segs) = concat segs

fromList :: [Int] -> Sort Int
fromList xs = foldr add empty xs
