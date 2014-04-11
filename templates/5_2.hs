module PDFS52 where

import Data.List (sort, nub)
import Test.QuickCheck
import Control.Applicative

type Queue a = ([a], [a])

inv :: Queue a -> Bool
inv (f, r) = not (null f) || (null r)

emptyQ :: Queue a
emptyQ = error "fill in the function body"

-- |
--
-- prop> inv $ check (q :: Queue Int)
check :: Queue a -> Queue a
check = error "fill in the function body"

headQ :: Queue a -> a
headQ = error "fill in the function body"

-- |
--
-- prop> (nonEmpty q) && (inv (q :: Queue Int)) ==> (inv $ tailQ q)
--
-- prop> (nonEmpty q) && (inv (q :: Queue Int)) ==> (elems $ tailQ q) == (tail $ elems q)
tailQ :: Queue a -> Queue a
tailQ = error "fill in the function body"

-- |
--
-- prop> (inv (q :: Queue Int)) ==> (inv $ put x q)
--
-- prop> (inv (q :: Queue Int)) ==> (elems $ put x q) == (elems q ++ [x])
put :: a -> Queue a -> Queue a
put = error "fill in the function body"

data Op a = T | P a deriving Show

instance Arbitrary a => Arbitrary (Op a) where
  arbitrary = oneof [return T, P <$> arbitrary]

-- |
--
-- prop> inv q ==> inv $ foldr evalOp q (ops :: [Op Int])
evalOp :: Op a -> Queue a -> Queue a
evalOp T = tailQ
evalOp (P x) = put x

-- Utility functions

elems :: Queue a -> [a]
elems (f, r) = f ++ reverse r

nonEmpty :: Queue a -> Bool
nonEmpty (f, r) = not $ null $ f ++ r
