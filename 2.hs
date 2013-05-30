import Test.QuickCheck

suffixes [] = [[]]
suffixes l@(x:xs) = l : suffixes xs

data Tree a = E | T (Tree a) a (Tree a) deriving (Show, Eq)

instance (Ord a, Arbitrary a) => Arbitrary (Tree a) where
    arbitrary = fmap fromList arbitrary

prop_mem mem xs = all (\x -> mem x t) xs where
  _ = xs :: [Int]
  t = fromList xs

prop_mem2 x t = (member x t) == (member2 x t) where
  _ = x :: Int
  _ = t :: Tree Int

member :: Ord a => a -> Tree a -> Bool
member _ E = False
member x (T l v r) = if x < v then member x l
                              else if x > v then member x r
                              else True

insert :: Ord a => a -> Tree a -> Tree a
insert x E = T E x E
insert x t@(T l v r) = if x < v then T (insert x l) v r
                                else if x > v then T l v (insert x r)
                                else t

fromList xs = foldr insert E xs

member2 :: Ord a => a -> Tree a -> Bool
member2 x t = member2' x t Nothing where
  member2' x E (Just v) = (x == v)
  member2' x E Nothing = False
  member2' x (T l v r) m = if x < v then member2' x l m
                                    else member2' x r (Just v)

prop_ins ins x t = member x (insert x t) where
  _ = t :: Tree Int
  _ = x :: Int

prop_ins2 x t = (insert x t) == (insert2 x t) where
  _ = x :: Int
  _ = t :: Tree Int

insert2 :: Ord a => a -> Tree a -> Tree a
insert2 x t = maybe t id $ ins' x t where
  ins' :: Ord a => a -> Tree a -> Maybe (Tree a)
  ins' x E = Just (T E x E)
  ins' x (T l v r) = if x < v then fmap (\l' -> T l' v r) (ins' x l)
                              else if x > v then fmap (\r' -> T l v r') (ins' x r)
                              else Nothing

insert3 :: Ord a => a -> Tree a -> Tree a
insert3 x t = maybe t id $ ins' x t Nothing where
  ins' :: Ord a => a -> Tree a -> Maybe a -> Maybe (Tree a)
  ins' x E (Just v) = if x == v then Nothing
                                else Just (T E x E)
  ins' x E Nothing = Just (T E x E)
  ins' x (T l v r) m = if x < v then fmap (\l' -> T l' v r) (ins' x l m)
                                else fmap (\r' -> T l v r') (ins' x r (Just v))

prop_complete x d = (d >= 0) && (d <= 20) ==> (complete x d) == (complete2 x d) where
  _ = x :: Int
  _ = d :: Int

complete :: a -> Int -> Tree a
complete x 0 = E
complete x d = T t x t where
  t = complete x (d - 1)

complete2 x d = (iterate (\t -> T t x t) E) !! d

prop_ins_size x t = (size $ insert x t) == (size t + (if member x t then 0 else 1)) where
  _ = x :: Int
  _ = t :: Tree Int

size E = 0
size (T l _ r) = size l + size r + 1

prop_create2 m = (m >= 0) ==> (size t1 == m) && (size t2 == m + 1) where
  _ = m :: Int
  (t1, t2) = create2 m 1

create2 0 x = (E, T E x E)
create2 m x = if odd m then (T t1 x t1, T t1 x t2) else (T t1 x t2, T t2 x t2) where
  (t1, t2) = create2 ((m - 1) `div` 2) x

balanced = fmap fst . create2
