{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

module Akrantiain.Sents_to_func
(sents_to_func
) where

import Akrantiain.Cook
import Akrantiain.Structure
import Akrantiain.Errors
import Control.Monad(forM)


sents_to_func :: Set Sentence -> (Either SemanticError (Input -> Output))
sents_to_func sents = do
 foobar <- mysterious sents
 cookBy foobar
 
data Sentence' = Conversion' (Array (Options, Phoneme)) | Define' Identifier Options deriving(Show, Eq, Ord)

-- check_length :: Sentence -> Either SemanticError Sentence'
-- check_length (Define ide opt) = Right $ Define' ide opt
-- check_length c@(Conversion a b) = check_length' a b (toSource c)

-- check_length' :: Array Options -> Array Phoneme -> String -> Either SemanticError Sentence'
-- check_length' [] [] _ = Right $ Conversion' []
-- check_length' [] _ msg = Left $ E{errNum = 100, errStr = "" ++ msg}

isConcrete :: Options -> Either SemanticError Bool
isConcrete o@(F terms)
 | all isConcreteTerm terms = Right True
 | all (not . isConcreteTerm) terms = Right False
 | otherwise = Left $ E{errNum = 102, errStr = "mix of concrete and non-concrete terms within {" ++ toSource o ++ "}"}


check_consistency :: Sentence -> Either SemanticError Sentence'
check_consistency (Define (Id ide) options) = do
 isconc <- isConcrete options
 if isconc then return $ Define' (Id ide) options else Left $ E{errNum = 101, errStr = "non-concrete term used as a candidate of identifier {"++ ide ++"}"}
check_consistency s@(Conversion options_arr phoneme_arr) = do
 optarr2 <- forM options_arr $ \options -> do
  bool <- isConcrete options
  return (options, bool)
 let merged = merge (Slash "") optarr2 phoneme_arr
 case merged of
  Just arr -> return $ Conversion' arr
  Nothing -> Left $ E{errNum = 103, errStr = "mismatched number of concrete terms in left- and right-hand side of:" ++ toSource s}

merge :: b -> [(a, Bool)] -> [b] -> Maybe [(a,b)]
merge _ [] [] = Just []
merge _ [] _ = Nothing
merge emp ((a, False):xs) ys = ((a, emp):) <$> merge emp xs ys
merge _ ((_, True):_) [] = Nothing
merge emp ((a, True):xs) (y:ys) = ((a,y):) <$> merge emp xs ys


mysterious :: Set Sentence -> Either SemanticError Fixme3
mysterious sents = do 
 sents' <- forM sents $ \sent -> do 
  check_consistency sent
 Right Fixme3

-- sents' :: [Sentence']
