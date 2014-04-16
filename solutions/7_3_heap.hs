module PDFS73 where

import Data.List (sort)
import Testing

data Tree a = Node {root :: a, children :: [Tree a]}
  deriving (Show, Eq)
data Digit a = Zero | One (Tree a) deriving (Show, Eq)
type Schedule a = [[Digit a]]
type Heap a = ([Digit a], Schedule a)

empty :: Heap a
empty = ([], [])

link :: Ord a => Tree a -> Tree a -> Tree a
link t1@(Node x1 c1) t2@(Node x2 c2) =
  if x1 <= x2
    then Node x1 (t2:c1)
    else Node x2 (t1:c2)

insTree :: Ord a => Tree a -> [Digit a] -> [Digit a]
insTree t [] = [One t]
insTree t (Zero:ds) = (One t):ds
insTree t (One t':ds) = Zero:(insTree (link t t') ds)

exec :: Schedule a -> Schedule a
exec [] = []
exec ((One _:_):sched) = sched
exec ((Zero:job):sched) = job:sched
exec _ = error "invalid schedule"

-- | Insert an element in to a heap
--
-- >>> prop $ \x xs -> (elems $ insert x $ heap xs) == (sort $ x:xs)
insert :: Ord a => a -> Heap a -> Heap a
insert x (ds, sched) = (ds', exec $ exec (ds':sched)) where
  ds' = insTree (Node x []) ds

removeMinTree :: Ord a => [Digit a] -> (Tree a, [Digit a])
removeMinTree [] = error "removeMinTree on an empty heap"
removeMinTree [One t] = (t, [])
removeMinTree (Zero:ds) = (t', (Zero:ds')) where
  (t', ds') = removeMinTree ds
removeMinTree ((One t@(Node x _)):ds) =
  let (t'@(Node x' _), ds') = removeMinTree ds in
  if x <= x'
    then (t, Zero:ds)
    else (t', (One t):ds')

-- |
--
-- >>> prop $ \(NonEmpty xs) -> (findMin $ heap xs) == (minimum xs)
findMin :: Ord a => Heap a -> a
findMin (ds, _) = x where
  (Node x _, _) = removeMinTree ds

-- |
--
-- >>> prop $ \(NonEmpty xs) -> (elems . deleteMin $ heap xs) == (tail $ sort xs)
deleteMin :: Ord a => Heap a => Heap a
deleteMin (ds, _) = (normalize ds2, []) where
  (Node _ c, ds1) = removeMinTree ds
  ds2 = mrg (map One (reverse c)) ds1

mrg :: Ord t => [Digit t] -> [Digit t] -> [Digit t]
mrg ds1 [] = ds1
mrg [] ds2 = ds2
mrg (Zero:ds1) (d:ds2) = d:(mrg ds1 ds2)
mrg (d:ds1) (Zero:ds2) = d:(mrg ds1 ds2)
mrg ((One t1):ds1) ((One t2):ds2) =
  Zero:(insTree (link t1 t2) (mrg ds1 ds2))

normalize :: [Digit a] -> [Digit a]
normalize [] = []
normalize ds@(_:ds') = seq (normalize ds') ds

elemsT :: Tree a -> [a]
elemsT (Node x c) = x:(concatMap elemsT c)

elemsD :: Digit a -> [a]
elemsD Zero = []
elemsD (One t) = elemsT t

elems :: Ord a => Heap a -> [a]
elems (ds, _) = sort $ concatMap elemsD ds

fromList :: (Ord a) => [a] -> Heap a
fromList xs = foldr insert empty xs

-- |
--
-- >>> prop $ \xs -> sort xs == (elems $ heap xs)
heap :: [Int] -> Heap Int
heap = fromList
