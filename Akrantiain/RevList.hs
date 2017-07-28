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

