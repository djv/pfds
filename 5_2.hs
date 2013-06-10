{-# LANGUAGE TemplateHaskell #-}

import Data.List (sort, nub)
import Test.QuickCheck
import Test.QuickCheck.All
import Test.LazySmallCheck (smallCheck)
import Control.Applicative

runTests = $quickCheckAll

type Queue a = ([a], [a])

emptyQ = ([], [])

check :: Queue a -> Queue a
check ([], r) = (reverse r, [])
check q = q

headQ :: Queue a -> a
headQ (x:f, r) = x

tailQ :: Queue a -> Queue a
tailQ (x:f, r) = check (f, r)
tailQ emptyQ = emptyQ

put :: a -> Queue a -> Queue a
put x (f, r) = check (f, x:r)

inv :: Queue a -> Bool
inv (f, r) = not (null f) || (null r)

prop_put f r x = (inv q) ==> (inv $ put (x::Int) q) where
  q = (f, r) :: Queue Int

prop_tailQ f r = (not $ null $ f ++ r) && (inv q) ==> (inv $ tailQ q) where
  q = (f, r) :: Queue Int

data Op a = T | P a deriving Show

instance Arbitrary a => Arbitrary (Op a) where
  arbitrary = oneof [return T, P <$> arbitrary]

evalOp :: Op a -> Queue a -> Queue a
evalOp T = tailQ
evalOp (P x) = put x

prop_seq_op ops = inv $ foldr evalOp emptyQ (ops :: [Op Int])
