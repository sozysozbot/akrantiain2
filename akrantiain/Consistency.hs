{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
-- checks the consistency of concrete or not; also handles the length
module Akrantiain.Consistency
(check_consistency
,Sentence'
,Rule(..)
,W(..)
,Boundary_
,Condition
,sentences_to_rules
) where

import Akrantiain.Structure
import Akrantiain.Errors
import Control.Monad(forM)

data Rule = R{leftneg :: Maybe(Condition), middle :: [ Either Boundary_ (Choose String, W)], rightneg :: Maybe(Condition)}
data W = W String | Dollar_ 
type Boundary_ = ()
type Condition = (String -> Bool)

data Sentence' = Conversion' (Array (Options, Phoneme)) | Define' Identifier Options deriving(Show, Eq, Ord)


sentences_to_rules :: [Sentence] -> Either SemanticError [Rule]
sentences_to_rules = undefined


isConcrete :: Options -> Either SemanticError Bool
isConcrete o@(F terms)
 | all isConcreteTerm terms = Right True
 | all (not . isConcreteTerm) terms = Right False
 | otherwise = Left $ E{errNum = 102, errStr = "mix of concrete and non-concrete terms within {" ++ toSource o ++ "}"}


check_consistency :: Sentence -> Either SemanticError Sentence'
check_consistency (Define (Id ide) ch_quote) = do
 let options = undefined ch_quote
 isconc <- isConcrete options
 if isconc then return $ Define' (Id ide) options else Left $ E{errNum = 101, errStr = "non-concrete term used as a candidate of identifier {"++ ide ++"}"}
check_consistency s@(Conversion options_arr phoneme_arr foo bar) = do
 optarr2 <- forM options_arr $ \ch_quote -> do
  let options = undefined ch_quote
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

