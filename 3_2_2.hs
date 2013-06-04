{-# LANGUAGE TemplateHaskell #-}

import Data.List (sort)
import Test.QuickCheck.All
import Test.LazySmallCheck

runTests = $quickCheckAll

{-ex. 3.6-}
data Tree a = Node {root :: a, children :: [Tree a]} deriving (Eq, Show)
type Rank = Int
type Heap a = [(Rank, Tree a)]

link t1@(Node x1 c1) t2@(Node x2 c2) =
  if x1 < x2 then Node x1 (t2 : c1)
             else Node x2 (t1 : c2)

insert x h = insTree (0, (Node x [])) h

insTree :: Ord a => (Rank, Tree a) -> Heap a -> Heap a
insTree rt [] = [rt]
insTree rt@(r, t) ts@((r', t'):ts') = if r < r' then rt:ts
                                                else insTree (r+1, (link t t')) ts'

fromList xs = foldr insert [] xs

elems :: Ord a => Heap a -> [a]
elems ts = sort $ concatMap (elemsT . snd) ts where
  elemsT :: Tree a -> [a]
  elemsT (Node x cs) = x:(concatMap elemsT cs)

prop_fromList xs = sort xs == (elems $ fromList (xs :: [Int]))

prop_fromList_rank_incr xs = sort rs == rs where
  rs = map fst $ fromList (xs :: [Int])

merge h1 [] = h1
merge [] h2 = h2
merge h1@((r1, t1):ts1) h2@((r2, t2):ts2) = if r1 < r2
  then (r1, t1):(merge ts1 h2)
  else if r1 > r2
    then (r2, t2):(merge h1 ts2)
    else insTree (r1+1, (link t1 t2)) (merge ts1 ts2)

prop_merge xs1 xs2 = sort (xs1 ++ xs2) == elems (merge h1 h2) where
  h1 = fromList (xs1 :: [Int])
  h2 = fromList (xs2 :: [Int])

removeMinTree :: Ord a => Heap a -> ((Rank, Tree a), Heap a)
removeMinTree [rt] = (rt, [])
removeMinTree (rt@(_, t):ts) = if root t < root t' then (rt, ts) else (rt', rt:ts') where
  (rt'@(_, t'), ts') = removeMinTree ts

findMin :: Ord a => Heap a -> a
findMin h = root . snd . fst $ removeMinTree h

prop_findMin xs = (not $ null (xs :: [Int])) ==> (findMin $ fromList xs) == (minimum xs)

deleteMin h = merge (zip [r,r-1..1] (reverse cs)) h2 where
  ((r, Node _ cs), h2) = removeMinTree h

prop_deleteMin xs = (not $ null (xs :: [Int])) ==>
  (elems . deleteMin $ fromList xs) == (tail $ sort xs)

{-ex. 3.5-}
findMin2 ts = minimum $ map (root . snd) ts

prop_findMin2_eq xs = (not $ null (xs :: [Int])) ==> (findMin h) == (findMin2 h) where
  h = fromList xs
