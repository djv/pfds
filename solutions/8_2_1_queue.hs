module PFDS821 where

import Testing

data RotationState a =
  Idle
  | Reversing Int [a] [a] [a] [a]
  | Appending Int [a] [a]
  | Done [a] deriving (Eq, Show)

type Queue a = (Int, [a], RotationState a, Int, [a])

exec :: RotationState a -> RotationState a
exec (Reversing ok (x:f) f' (y:r) r') = Reversing (ok+1) f (x:f') r (y:r')
exec (Reversing ok [] f' [y] r') = Appending ok f' (y:r')
exec (Appending 0 _ r') = Done r'
exec (Appending ok (x:f') r') = Appending (ok-1) f' (x:r')
exec state = state

invalidate :: RotationState a -> RotationState a
invalidate (Reversing ok f f' r r') = Reversing (ok-1) f f' r r'
invalidate (Appending 0 _ (_:r')) = Done r'
invalidate (Appending ok f' r') = Appending (ok-1) f' r'
invalidate state = state

exec2 :: Queue a -> Queue a
exec2 (lenf, f, state, lenr, r) =
  case exec (exec state) of
    Done f2 -> (lenf, f2, Idle, lenr, r)
    state2 -> (lenf, f, state2, lenr, r)

check :: Queue a -> Queue a
check q@(lenf, f, _, lenr, r) =
  if lenr <= lenf
    then exec2 q
    else exec2 (lenf+lenr, f, (Reversing 0 f [] r []), 0, [])

empty :: Queue a
empty = (0, [], Idle, 0, [])

-- |
--
-- >>> prop $ \x xs -> x `elem` (elems $ put x $ fromList xs)
put :: a -> Queue a -> Queue a
put x (lenf, f, state, lenr, r) = check (lenf, f, state, lenr+1, x:r)

-- |
--
-- >>> prop $ \(NonEmpty xs) -> headQ (fromList xs) == head xs
headQ :: Queue a -> a
headQ (_, [], _, _, _) = error "headQ on an empty queue"
headQ (_, x:_, _, _, _) = x

-- |
--
-- >>> prop $ \xs -> not (null xs) ==> (elems $ tailQ $ fromList xs) == tail xs
tailQ :: Queue a -> Queue a
tailQ (_, [], _, _, _) = empty
tailQ (lenf, _:f, state, lenr, r) = check (lenf-1, f, invalidate state, lenr, r)

-- |
--
-- >>> prop $ \xs -> elems (fromList xs) == xs
elems :: Queue a -> [a]
elems (_, f, Idle, _, r) = f ++ reverse r
elems (_, f, Reversing _ _ _ rr rr', _, r) = f ++ reverse rr ++ rr' ++ reverse r
elems (_, _, Appending l f' r', _, r) = reverse (take l f') ++ r' ++ reverse r
elems _ = error "invalid queue"

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
