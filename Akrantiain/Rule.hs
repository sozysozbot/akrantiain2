{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Akrantiain.Rule
(Rule(..)
,W(..)
,Boundary_
,Condition
,Punctuation
,no
,Environment(..)
,Rules
) where
import Prelude hiding (undefined)
import Akrantiain.Structure
import qualified Data.Map as M

data Rule = R{leftneg :: Maybe(Condition), middle :: [ Either Boundary_ (Choose String, W)], rightneg :: Maybe(Condition)}
data W = W String | Dollar_ 
type Boundary_ = ()
type Condition = (String -> Bool)
type Punctuation = [Char]
data Environment = Env{pun :: Punctuation, bools :: M.Map Identifier Bool}
type Rules = (Environment,[Rule])

no :: Choose String -> Condition
no (Ch foo) str
 | null str = True
 | str `elem` foo = False
 | otherwise = True

