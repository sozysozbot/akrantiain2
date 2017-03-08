{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

module Akrantiain.Sents_to_rules
(sents_to_func
) where

import Akrantiain.Structure
import Akrantiain.Errors
import Control.Monad(forM)
import Akrantiain.Pattern_match
import Akrantiain.Consistency


type Input = String
type Output = Either RuntimeError String



sents_to_func :: Set Sentence -> (Either SemanticError (Input -> Output))
sents_to_func sents = do
 sents' <- forM sents $ \sent -> do 
  check_consistency sent
 rules <- mysterious sents'
 return $ cook (undefined,rules)

mysterious :: Set Sentence' -> Either SemanticError [Rule]
mysterious sents' = undefined
