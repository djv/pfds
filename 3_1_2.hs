{-# LANGUAGE TemplateHaskell #-}

{-import Test.QuickCheck-}
import Data.List (sort)
{-import Test.SmallCheck-}
import Test.LazySmallCheck
import Test.QuickCheck.All

runTests = $quickCheckAll

data Heap a = E | H Int (Heap a) a (Heap a) deriving (Eq, Show)

merge E h = h
merge h E = h
merge h1@(H r1 a1 x1 b1) h2@(H r2 a2 x2 b2) =
  if x1 < x2 then makeT (rank a1) (rank b1 + r2) x1 a1 (merge b1 h2)
             else makeT (rank a2) (r1 + rank b2) x2 a2 (merge h1 b2)

rank E = 0
rank (H r _ _ _) = r

makeT ra rb x a b = if ra > rb then H (1 + rb + ra) a x b
                               else H (1 + ra + rb) b x a

insert x h = merge (singleton x) h

findMin (H _ _ x _) = x

deleteMin (H _ a _ b) = merge a b

fromList xs = foldr insert E xs

toList E = []
toList (H _ l v r) = toList l ++ [v] ++ toList r

elems h = sort $ toList h

prop_fromlist xs = (elems . fromList $ xs) == sort xs where
  _ = xs :: [Int]

prop_fromlist_rank xs = checkRank $ fromList xs where
  _ = xs :: [Int]

prop_fromlist_min xs = (not $ null xs) ==> (findMin $ fromList xs) == (minimum xs) where
  _ = xs :: [Int]

prop_del_rank xs = (not $ null xs) ==> checkRank . deleteMin $ fromList xs where
  _ = xs :: [Int]

prop_rank xs = (calcRank h) == (rank h) where
  _ = xs :: [Int]
  h = fromList xs

prop_insert2_eq x xs = (elems $ insert x h) == (elems $ insert2 x h) where
  _ = x :: Int
  _ = xs :: [Int]
  h = fromList xs

prop_insert_add x xs = (elems $ insert x h) == (sort $ x:xs) where
  _ = x :: Int
  _ = xs :: [Int]
  h = fromList xs

prop_insert_min x xs = (findMin $ insert x h) == (minimum $ x:xs) where
  _ = x :: Int
  _ = xs :: [Int]
  h = fromList xs

prop_fromlist2_eq xs = (elems . fromList $ xs) == (elems . fromList2 $ xs) where
  _ = xs :: [Int]

prop_fromlist2_rank xs = checkRank $ fromList2 xs where
  _ = xs :: [Int]

prop_fromlist2_min xs = (not $ null xs) ==> (findMin $ fromList2 xs) == (minimum xs) where
  _ = xs :: [Int]

insert2 x E = singleton x
insert2 x h@(H m l v r) = if x < v then H 0 h x E
                                   else H m (insert2 x l) v r

checkRank E = True
checkRank (H _ l _ r) = rank l >= rank r && checkRank l && checkRank r

calcRank E = 0
calcRank (H _ l v r) = 1 + calcRank l + calcRank r

singleton x = H 1 E x E

fromList2 [] = E
fromList2 xs = head . f $ map singleton xs where
  f [] = []
  f [h] = [h]
  f (h1:h2:hs) = f $ (merge h1 h2) : (f hs)
