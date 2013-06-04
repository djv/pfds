{-# LANGUAGE TemplateHaskell #-}

import Data.List (sort)
import Test.QuickCheck.All
import Test.LazySmallCheck

runTests = $quickCheckAll

data Tree a = Node {rank :: Int, root :: a, children :: [Tree a]} deriving (Eq, Show)
type Heap a = [Tree a]

link t1@(Node r x1 c1) t2@(Node _ x2 c2) =
  if x1 < x2 then Node (r + 1) x1 (t2 : c1)
             else Node (r + 1) x2 (t1 : c2)

insert x h = insTree (Node 0 x []) h

insTree t [] = [t]
insTree t ts@(t':ts') = if rank t < rank t' then t:ts
                                            else insTree (link t t') ts'

fromList xs = foldr insert [] xs

elems [] = []
elems ((Node _ x cs):ts) = sort $ x:(elems cs ++ elems ts)

prop_fromList xs = sort xs == (elems $ fromList (xs :: [Int]))

prop_fromList_rank_incr xs = sort rs == rs where
  rs = map rank $ fromList (xs :: [Int])

merge h1 [] = h1
merge [] h2 = h2
merge h1@(h1':hs1) h2@(h2':hs2) = if rank h1' < rank h2'
  then h1':(merge hs1 h2)
  else if rank h1' > rank h2'
    then h2':(merge h1 hs2)
    else insTree (link h1' h2') (merge hs1 hs2)

prop_merge xs1 xs2 = sort (xs1 ++ xs2) == elems (merge h1 h2) where
  h1 = fromList (xs1 :: [Int])
  h2 = fromList (xs2 :: [Int])

removeMinTree [t] = (t, [])
removeMinTree (t:ts) = if root t < root t' then (t, ts) else (t', t:ts') where
  (t', ts') = removeMinTree ts

findMin h = root . fst $ removeMinTree h

prop_findMin xs = (not $ null (xs :: [Int])) ==> (findMin $ fromList xs) == (minimum xs)

deleteMin h = merge (reverse cs) h2 where
  (Node _ _ cs , h2) = removeMinTree h

prop_deleteMin xs = (not $ null (xs :: [Int])) ==>
  (elems . deleteMin $ fromList xs) == (tail $ sort xs)

{-ex. 3.5-}
findMin2 ts = minimum $ map root ts

prop_findMin2_eq xs = (not $ null (xs :: [Int])) ==> (findMin h) == (findMin2 h) where
  h = fromList xs
