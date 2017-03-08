{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
-- checks the consistency of concrete or not; also handles the length
module Akrantiain.Consistency
(Rule(..)
,W(..)
,Boundary_
,Condition
,sentences_to_rules
,Punctuation
) where

import Akrantiain.Structure
import Akrantiain.Errors
import Control.Monad

data Rule = R{leftneg :: Maybe(Condition), middle :: [ Either Boundary_ (Choose String, W)], rightneg :: Maybe(Condition)}
data W = W String | Dollar_ 
type Boundary_ = ()
type Condition = (String -> Bool)
type Punctuation = [Char]


sentences_to_rules :: [Sentence] -> Either SemanticError (Punctuation,[Rule])
sentences_to_rules = undefined

