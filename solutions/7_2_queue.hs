module PDFS72 where

import Testing

type RTQueue a = ([a], [a], [a])

inv :: RTQueue a -> Bool
inv (f, r, s) = length f == length r + length s

empty :: RTQueue a
empty = ([], [], [])

-- |
--
-- >>> prop $ \xs -> isEmpty (fromList xs) == null xs
isEmpty :: RTQueue a -> Bool
isEmpty ([], _, _) = True
isEmpty _ = False

rotate :: RTQueue a -> [a]
rotate ([], (y:[]), s) = y:s
rotate (x:xs, y:ys, s) = x:(rotate (xs, ys, y:s))
rotate _ = error "invalid call to rotate"

exec :: RTQueue a -> RTQueue a
exec (f, r, (_:s)) = (f, r, s)
exec q@(_, _, []) = (f', [], f') where
  f' = rotate q

-- |
--
-- >>> prop $ \x xs -> x == (last $ elems $ put x $ fromList xs)
put :: a -> RTQueue a -> RTQueue a
put x (f, r, s) = exec (f, x:r, s)

headQ :: RTQueue a -> a
headQ ([], _, _) = error "head on an empty queue"
headQ ((x:_), _, _) = x

tailQ :: RTQueue a -> RTQueue a
tailQ ([], _, _) = empty
tailQ ((_:f), r, s) = exec (f, r, s)

-- |
--
-- >>> prop $ \xs -> size (fromList xs) == length xs
size :: RTQueue a -> Int
size (_, r, s) = 2 * (length r) + length s

-- |
--
-- >>> prop $ \xs -> elems (fromList xs) == xs
fromList :: [Int] -> RTQueue Int
fromList = foldl (flip put) empty

elems :: RTQueue a -> [a]
elems (f, r, _) = f ++ reverse r

data Op a = T | P a deriving Show

instance Arbitrary a => Arbitrary (Op a) where
  arbitrary = oneof [return T, fmap P arbitrary]

-- |
--
-- >>> prop $ \xs ops -> inv $ foldr evalOp (fromList xs) (ops :: [Op Int])
--
-- >>> prop $ \xs ops -> (foldr evalOpL xs ops) == (elems $ foldr evalOp (fromList xs) ops)
evalOp :: Op a -> RTQueue a -> RTQueue a
evalOp T = tailQ
evalOp (P x) = put x

evalOpL :: Op a -> [a] -> [a]
evalOpL T [] = []
evalOpL T xs = tail xs
evalOpL (P x) xs = xs ++ [x]
