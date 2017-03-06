{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

module Akrantiain.Sents_to_func
(sents_to_func
) where

import Akrantiain.Cook
import Akrantiain.Structure
import Akrantiain.Errors
import Control.Monad(forM)
import Akrantiain.Consistency

sents_to_func :: Set Sentence -> (Either SemanticError (Input -> Output))
sents_to_func sents = do
 foobar <- mysterious sents
 cookBy foobar

mysterious :: Set Sentence -> Either SemanticError Fixme3
mysterious sents = do 
 sents' <- forM sents $ \sent -> do 
  check_consistency sent
 Right Fixme3

-- sents' :: [Sentence']
