{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

module Akrantiain.Sents_to_rules
(sents_to_func
) where

import Akrantiain.Structure
import Akrantiain.Errors
import Control.Monad(forM)
import Akrantiain.Pattern_match
import Akrantiain.Rule


type Input = String
type Output = Either RuntimeError String



sents_to_func :: Set Sentence -> (Either SemanticError (Input -> Output))
sents_to_func sents = do
 (punct,rules) <- sentences_to_rules sents
 return $ cook (punct,rules)



sentences_to_rules :: [Sentence] -> Either SemanticError (Punctuation,[Rule])
sentences_to_rules = undefined

