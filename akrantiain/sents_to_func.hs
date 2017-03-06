{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

module Akrantiain.Sents_to_func
(sents_to_func
) where

import Akrantiain.Expand
import Akrantiain.Cook
import Akrantiain.Normalize
import Akrantiain.Structure


sents_to_func :: Set Sentence -> (Either SemanticError (Input -> Output))
sents_to_func sents = do
 conv2_arr <- expand sents
 foobar <- mysterious conv2_arr
 cookBy foobar

mysterious :: [Conv2] -> Either SemanticError Fixme3
mysterious cccccccc = Right Fixme3

