module Akrantiain.RevList
(RevList(..)
,toList,fromList
) where
import Prelude hiding (undefined)

newtype RevList a = Reverse{unReverse :: [a]} deriving(Show,Eq,Ord)

instance Functor RevList where
 fmap f (Reverse a) = Reverse (fmap f a)

toList :: RevList a -> [a]
toList (Reverse a) = reverse a

fromList :: [a] -> RevList a
fromList bs = Reverse (reverse bs)

instance Foldable RevList where
 foldr f z (Reverse arr) = foldl (flip f) z arr
--    foldr f z (Reverse [x1, x2, ..., xn])
-- == foldr f z . toList $ (Reverse [x1, x2, ..., xn])
-- == foldr f z (reverse [x1, x2, ..., xn])
-- == foldr f z [xn, ..., x2, x1] 
-- == xn `f` (xn_1 `f` ... (x1 `f` z)...)
-- ==  (... (z `g` x1) ... `g` xn_1) `g` xn where g = flip f
-- == foldl (flip f) z [x1, x2, ..., xn]
