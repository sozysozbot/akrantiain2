{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

module Akrantiain.Normalize
(normalize
,Fixme(..)
) where
import Akrantiain.Expand
import Akrantiain.Structure

data Conv3 = C3 { unC3 :: Array(Orthography', Phoneme)} deriving (Eq, Show, Ord)

two_to_three :: Conv2 -> Conv3
two_to_three (Conv [] []) = C3 []
two_to_three (Conv [] _ ) = error "not supposed to happen $&#"
two_to_three (Conv _  []) = error "not supposed to happen &)!"
two_to_three (Conv (Neg' a:xs) ys) = C3 $ (Neg' a, Slash "") : (unC3 . two_to_three)(Conv xs ys) 
two_to_three (Conv (Pos' a:xs) (y:ys)) = undefined


type Fixme = ()
normalize :: Set Conv2 -> Fixme
normalize = undefined


