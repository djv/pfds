import Test.QuickCheck
import Data.List (sort)
{-import Test.SmallCheck-}

data Heap a = E | H Int (Heap a) a (Heap a) deriving (Eq, Show)

merge E h = h
merge h E = h
merge h1@(H _ a1 x1 b1) h2@(H _ a2 x2 b2) = if x1 < x2 then makeT x1 a1 (merge b1 h2)
                                                       else makeT x2 a2 (merge h1 b2)

rank E = 0
rank (H r _ _ _) = r

makeT x a b = if rank a > rank b then H (rank b + 1) a x b
                                 else H (rank a + 1) b x a

insert x h = merge (H 1 E x E) h

findMin (H _ _ x _) = x

deleteMin (H _ a _ b) = merge a b

fromList xs = foldr insert E xs

toList E = []
toList (H _ l v r) = toList l ++ [v] ++ toList r

prop_fromlist xs = (sort . toList . fromList $ xs) == sort xs where
  _ = xs :: [Int]
