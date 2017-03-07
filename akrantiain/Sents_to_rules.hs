{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

module Akrantiain.Sents_to_rules
(sents_to_func
) where

import Akrantiain.Cook
import Akrantiain.Structure
import Akrantiain.Errors
import Control.Monad(forM)
import Akrantiain.Consistency

sents_to_func :: Set Sentence -> (Either SemanticError (Input -> Output))
sents_to_func sents = do
 sents' <- forM sents $ \sent -> do 
  check_consistency sent
 fixme3 <- mysterious sents'
 cookBy fixme3

mysterious :: Set Sentence' -> Either SemanticError Fixme3
mysterious sents' = undefined
