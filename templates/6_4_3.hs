module PDFS643 where

import Data.List

type Sort a = (Int, [[a]])

empty :: Sort a
empty = error "fill in the function body"

-- |
--
-- prop> mrg (sort (xs :: [Int])) (sort ys) == sort (xs ++ ys)
mrg :: Ord a => [a] -> [a] -> [a]
mrg = error "fill in the function body"

-- |
--
-- prop> x `elem` (elems $ add x $ fromList xs)
add :: Ord a => a -> Sort a -> Sort a
add = error "fill in the function body"

-- |
--
-- prop> sorted (fromList xs) == sort xs
sorted :: Ord a => (Int, [[a]]) -> [a]
sorted = error "fill in the function body"

-- Utility functions

elems :: Sort a -> [a]
elems (_, segs) = concat segs

fromList :: [Int] -> Sort Int
fromList xs = foldr add empty xs
