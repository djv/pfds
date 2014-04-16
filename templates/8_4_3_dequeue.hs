module PFDS843 where

import Testing

type RTDequeue a = (Int, [a], [a], Int, [a], [a])

c = 2
--c = 3

empty :: RTDequeue a
empty = error "fill in the function body"

exec1 = error "fill in the function body"

exec2 = error "fill in the function body"

rotateRev = error "fill in the function body"

rotateDrop = error "fill in the function body"

check :: RTDequeue a -> RTDequeue a
check = error "fill in the function body"

-- |
--
-- >>> prop $ \x xs -> x == (head $ elems $ cons x $ fromList xs)
cons :: a -> RTDequeue a -> RTDequeue a
cons = error "fill in the function body"

-- |
--
-- >>> prop $ \x xs -> x == (last $ elems $ snoc x $ fromList xs)
snoc :: a -> RTDequeue a -> RTDequeue a
snoc = error "fill in the function body"

headQ :: RTDequeue a -> a
headQ = error "fill in the function body"

lastQ :: RTDequeue a -> a
lastQ = error "fill in the function body"

-- |
--
-- >>> prop $ \(NonEmpty xs) -> tail xs == (elems $ tailQ $ fromList xs)
tailQ :: RTDequeue a -> RTDequeue a
tailQ = error "fill in the function body"

-- |
--
-- >>> prop $ \(NonEmpty xs) -> init xs == (elems $ initQ $ fromList xs)
initQ :: RTDequeue a -> RTDequeue a
initQ = error "fill in the function body"

-- | use cons
--
-- >>> prop $ \xs -> elems (fromList xs) == xs
fromList :: [Int] -> RTDequeue Int
fromList = error "fill in the function body"

-- | use snoc
--
-- >>> prop $ \xs -> elems (fromList xs) == elems (fromList2 xs)
fromList2 :: [Int] -> RTDequeue Int
fromList2 = error "fill in the function body"

elems :: RTDequeue a -> [a]

data Op a = T | I | C a | S a deriving Show

instance Arbitrary a => Arbitrary (Op a) where
  arbitrary = oneof [return T, return I, fmap C arbitrary, fmap S arbitrary]

-- |
--
-- >>> prop $ \xs ops -> (foldr evalOpL xs ops) == (elems $ foldr evalOp (fromList xs) ops)
evalOp :: Op a -> RTDequeue a -> RTDequeue a
evalOp T = tailQ
evalOp I = initQ
evalOp (C x) = cons x
evalOp (S x) = snoc x

evalOpL :: Op a -> [a] -> [a]
evalOpL T [] = []
evalOpL T xs = tail xs
evalOpL I [] = []
evalOpL I xs = init xs
evalOpL (C x) xs = x:xs
evalOpL (S x) xs = xs ++ [x]
