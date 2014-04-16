module PFDS821 where

import Testing

data RotationState a =
  Idle
  | Reversing Int [a] [a] [a] [a]
  | Appending Int [a] [a]
  | Done [a] deriving (Eq, Show)

type Queue a = (Int, [a], RotationState a, Int, [a])

exec :: RotationState a -> RotationState a
exec = error "fill in the function body"

invalidate :: RotationState a -> RotationState a
invalidate = error "fill in the function body"

exec2 :: Queue a -> Queue a
exec2 = error "fill in the function body"

check :: Queue a -> Queue a
check = error "fill in the function body"

empty :: Queue a
empty = error "fill in the function body"

-- |
--
-- >>> prop $ \x xs -> x `elem` (elems $ put x $ fromList xs)
put :: a -> Queue a -> Queue a
put = error "fill in the function body"

-- |
--
-- >>> prop $ \(NonEmpty xs) -> headQ (fromList xs) == head xs
headQ :: Queue a -> a
headQ = error "fill in the function body"

-- |
--
-- >>> prop $ \xs -> not (null xs) ==> (elems $ tailQ $ fromList xs) == tail xs
tailQ :: Queue a -> Queue a
tailQ = error "fill in the function body"

-- |
--
-- >>> prop $ \xs -> elems (fromList xs) == xs
elems :: Queue a -> [a]
elems = error "fill in the function body"

fromList :: [Int] -> Queue Int
fromList = foldl (flip put) empty

data Op a = T | P a deriving Show

instance Arbitrary a => Arbitrary (Op a) where
  arbitrary = oneof [return T, fmap P arbitrary]

-- |
--
-- >>> prop $ \xs ops -> (foldr evalOpL xs ops) == (elems $ foldr evalOp (fromList xs) ops)
evalOp :: Op a -> Queue a -> Queue a
evalOp T = tailQ
evalOp (P x) = put x

evalOpL :: Op a -> [a] -> [a]
evalOpL T [] = []
evalOpL T xs = tail xs
evalOpL (P x) xs = xs ++ [x]
