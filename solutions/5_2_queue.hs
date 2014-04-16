module PDFS52 where

import Data.List (sort, nub)
import Testing
import Control.Applicative

type Queue a = ([a], [a])

inv :: Queue a -> Bool
inv (f, r) = not (null f) || (null r)

emptyQ :: Queue a
emptyQ = ([], [])

-- |
--
-- >>> prop $ \q -> inv $ check (q :: Queue Int)
check :: Queue a -> Queue a
check ([], r) = (reverse r, [])
check q = q

headQ :: Queue a -> a
headQ (x:_, _) = x
headQ ([], []) = error "headQ on an empty queue"
headQ ([], _) = error "invalid queue"

-- |
--
-- >>> prop $ \q -> (nonEmpty q) && (inv (q :: Queue Int)) ==> (inv $ tailQ q)
--
-- >>> prop $ \q -> (nonEmpty q) && (inv (q :: Queue Int)) ==> (elems $ tailQ q) == (tail $ elems q)
tailQ :: Queue a -> Queue a
tailQ (_:f, r) = check (f, r)
tailQ ([], []) = emptyQ
tailQ ([], _) = error "invalid queue"

-- |
--
-- >>> prop $ \x q -> (inv (q :: Queue Int)) ==> (inv $ put x q)
--
-- >>> prop $ \x q -> (inv (q :: Queue Int)) ==> (elems $ put x q) == (elems q ++ [x])
put :: a -> Queue a -> Queue a
put x (f, r) = check (f, x:r)

data Op a = T | P a deriving Show

instance Arbitrary a => Arbitrary (Op a) where
  arbitrary = oneof [return T, P <$> arbitrary]

-- |
--
-- >>> prop $ \q ops -> inv q ==> inv $ foldr evalOp q (ops :: [Op Int])
evalOp :: Op a -> Queue a -> Queue a
evalOp T = tailQ
evalOp (P x) = put x

-- Utility functions

elems :: Queue a -> [a]
elems (f, r) = f ++ reverse r

nonEmpty :: Queue a -> Bool
nonEmpty (f, r) = not $ null $ f ++ r
