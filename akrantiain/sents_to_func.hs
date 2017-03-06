{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

module Akrantiain.Sents_to_func
(sents_to_func
) where

import Akrantiain.Cook
import Akrantiain.Structure
import Akrantiain.Resolve_definitions


sents_to_func :: Set Sentence -> (Either SemanticError (Input -> Output))
sents_to_func sents = do
 foobar <- mysterious sents
 cookBy foobar

mysterious :: Set Sentence -> Either SemanticError Fixme3
mysterious cccccccc = Right Fixme3

