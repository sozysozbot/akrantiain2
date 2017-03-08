{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

module Akrantiain.Sents_to_rules
(sents_to_func
) where

import Akrantiain.Structure
import Akrantiain.Errors
import Control.Monad(forM,when)
import Akrantiain.Pattern_match
import Akrantiain.Rule
import Data.Either(lefts, rights)
import Data.List(group, sort, intercalate)
import qualified Data.Map as M

type Input = String
type Output = Either RuntimeError String



sents_to_func :: Set Sentence -> (Either SemanticError (Input -> Output))
sents_to_func sents = do
 (punct,rules) <- sentences_to_rules sents
 return $ cook (punct,rules)

-- data Conversion = Conversion {middle::(Array Select), phons:: (Array Phoneme), lneg ::Maybe Select, rneg::Maybe Select}
-- data Select = Boundary2 | Iden Identifier | Pipe (Choose Quote) deriving(Show, Eq, Ord)

sentences_to_rules :: [Sentence] -> Either SemanticError (Punctuation,[Rule])
sentences_to_rules sents = do
 let (convs, defs) = (lefts sents, map (\(Define a b) -> (a,b)) $ rights sents)
 let duplicates = (map head . filter (\x -> length x > 1) . group . sort . map fst) defs
 when (not $ null duplicates) $ Left E{errNum = 334, errStr = "duplicate definition regarding identifier(s) {" ++ intercalate "}, {" (map unId duplicates) ++ "}"}
 let defs_ = M.fromList defs
 rules <- forM convs $ \conv -> do
  undefined
 undefined
 -- "punctuation" `M.lookup`








