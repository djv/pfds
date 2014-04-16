module PFDS843 where

import Testing

type RTDequeue a = (Int, [a], [a], Int, [a], [a])

c = 3

empty :: RTDequeue a
empty = (0, [], [], 0, [], [])

exec1 [] = []
exec1 (x:s) = s

exec2 = exec1 . exec1

rotateRev [] r a = reverse r ++ a
rotateRev (x:f) r a = x:(rotateRev f (drop c r) (reverse (take c r) ++ a))

rotateDrop f j r =
  if j < c
    then rotateRev f (drop j r) []
    else (head f):(rotateDrop (tail f) (j-c) (drop c r))

check :: RTDequeue a -> RTDequeue a
check q@(lenf, f, _, lenr, r, _) =
  if lenf > c*lenr + 1
    then let i = (lenf + lenr) `div` 2
             j = lenf + lenr - i
             f' = take i f
             r' = rotateDrop r i f
         in (i, f', f', j, r', r')
    else if lenr > c*lenf + 1
          then let j = (lenf + lenr) `div` 2
                   i = lenf + lenr - j
                   r' = take j r
                   f' = rotateDrop f j r
               in (i, f', f', j, r', r')
          else q

-- |
--
-- >>> prop2 $ \x xs -> x == (head $ elems $ cons x $ fromList xs)
cons :: a -> RTDequeue a -> RTDequeue a
cons x (lenf, f, sf, lenr, r, sr) =
  check (lenf + 1, x:f, exec1 sf, lenr, r, exec1 sr)

-- |
--
-- >>> prop2 $ \x xs -> x == (last $ elems $ snoc x $ fromList xs)
snoc :: a -> RTDequeue a -> RTDequeue a
snoc x (lenf, f, sf, lenr, r, sr) =
  check (lenf, f, exec1 sf, lenr + 1, x:r, exec1 sr)

headQ :: RTDequeue a -> a
headQ (_, [], _, _, [], _) = error "head on empty dequeue"
headQ (_, [], _, _, x:_, _) = x
headQ (_, x:_, _, _, _, _) = x

lastQ :: RTDequeue a -> a
lastQ (_, [], _, _, [], _) = error "last on empty dequeue"
lastQ (_, x:_, _, _, [], _) = x
lastQ (_, _, _, _, x:_, _) = x

-- |
--
-- >>> prop2 $ \(NonEmpty xs) -> tail xs == (elems $ tailQ $ fromList xs)
tailQ :: RTDequeue a -> RTDequeue a
tailQ (_, [], _, _, [], _) = empty
tailQ (_, [], _, _, _:_, _) = empty
tailQ (lenf, _:f', sf, lenr, r, sr) = check (lenf - 1, f', exec2 sf, lenr, r, exec2 sr)

-- |
--
-- >>> prop2 $ \(NonEmpty xs) -> init xs == (elems $ initQ $ fromList xs)
initQ :: RTDequeue a -> RTDequeue a
initQ (_, [], _, _, [], _) = empty
initQ (_, _:_, _, _, [], _) = empty
initQ (lenf, f, sf, lenr, _:r', sr) = check (lenf, f, exec2 sf, lenr - 1, r', exec2 sr)

-- |
--
-- >>> prop2 $ \xs -> elems (fromList xs) == xs
fromList :: [Int] -> RTDequeue Int
fromList = foldr cons empty

-- |
--
-- >>> prop2 $ \xs -> elems (fromList xs) == elems (fromList2 xs)
fromList2 :: [Int] -> RTDequeue Int
fromList2 = foldl (flip snoc) empty

elems :: RTDequeue a -> [a]
elems (_, f, _, _, r, _) = f ++ reverse r

data Op a = T | I | C a | S a deriving Show

instance Arbitrary a => Arbitrary (Op a) where
  arbitrary = oneof [return T, return I, fmap C arbitrary, fmap S arbitrary]

-- |
--
-- >>> prop2 $ \xs ops -> (foldr evalOpL xs ops) == (elems $ foldr evalOp (fromList xs) ops)
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
