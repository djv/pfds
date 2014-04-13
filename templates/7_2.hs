module PDFS72 where

import Test.QuickCheck

type RTQueue a = ([a], [a], [a])

inv :: RTQueue a -> Bool
inv (f, r, s) = length f == length r + length s

empty :: RTQueue a
empty = error "fill in the function body"

-- |
--
-- prop> isEmpty (fromList xs) == null xs
isEmpty :: RTQueue a -> Bool
isEmpty = error "fill in the function body"

rotate :: RTQueue a -> [a]
rotate = error "fill in the function body"

exec :: RTQueue a -> RTQueue a
exec = error "fill in the function body"

-- |
--
-- prop> x `elem` (elems $ put x $ fromList xs)
put :: a -> RTQueue a -> RTQueue a
put = error "fill in the function body"

headQ :: RTQueue a -> a
headQ = error "fill in the function body"

tailQ :: RTQueue a -> RTQueue a
tailQ = error "fill in the function body"

-- |
--
-- prop> size (fromList xs) == length xs
size :: RTQueue a -> Int
size = error "fill in the function body"

-- |
--
-- prop> elems (fromList xs) == xs
fromList :: [Int] -> RTQueue Int
fromList = error "fill in the function body"

elems :: RTQueue a -> [a]
elems (f, r, _) = f ++ reverse r

data Op a = T | P a deriving Show

instance Arbitrary a => Arbitrary (Op a) where
  arbitrary = oneof [return T, fmap P arbitrary]

-- |
--
-- prop> inv $ foldr evalOp (fromList xs) (ops :: [Op Int])
evalOp :: Op a -> RTQueue a -> RTQueue a
evalOp T = tailQ
evalOp (P x) = put x
