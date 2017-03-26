{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

module Akrantiain.Sents_to_rules
(sentsToFunc
,Input
,Output
) where
import Prelude hiding (undefined)
import Akrantiain.Structure
import Akrantiain.Errors
import Akrantiain.Rule
import Control.Monad(forM,unless)
import Akrantiain.Pattern_match
import Data.List(group, sort, intercalate)
import qualified Data.Map as M

type Input = String
type Output = Either RuntimeError String



sentsToFunc :: Set Sentence -> Either SemanticError (Input -> Output)
sentsToFunc sents = do
 (env,rules) <- sentencesToRules sents
 return $ cook (env,rules)

split3 :: [Sentence] -> ([Conversion],[Identifier],[Define])
split3 [] = ([],[],[])
split3 (Left'   c:xs) = let (cs,is,ds) = split3 xs in (c:cs,is,ds)
split3 (Middle' i:xs) = let (cs,is,ds) = split3 xs in (cs,i:is,ds)
split3 (Right'  d:xs) = let (cs,is,ds) = split3 xs in (cs,is,d:ds)

sentencesToRules :: [Sentence] -> Either SemanticError (Environment,[Rule])
sentencesToRules sents = do
 let (convs, vars_pre, defs_pre) = split3 sents
 let defs = map (\(Define a b) -> (a,b)) defs_pre
 let vars = M.fromList $ zip vars_pre (repeat ())
 let duplicates = (map head . filter (\x -> length x > 1) . group . sort . map fst) defs
 unless (null duplicates) $ Left E{errNum = 334, errStr = "duplicate definition regarding identifier(s) {" ++ intercalate "}, {" (map unId duplicates) ++ "}"}
 let defs_ = M.fromList defs
 let punct = case Id "PUNCTUATION" `M.lookup` defs_ of{Nothing -> "";
  Just (Ch arr) -> arr >>= unQ} -- FIXME: THIS CONCAT ISN'T RIGHT
 rules <- forM convs $ \conv@Conversion{lneg=left, mid=midd, rneg=right, phons=phonemes} -> do
  let solve = resolveSelect defs_
  left'  <- traverse solve left
  right' <- traverse solve right
  midd'  <- traverse solve midd -- midd' :: [Either Boundary_ (Choose String)]
  case zipEither midd' (map phonToW phonemes) of
   Nothing -> Left E{errNum = 333, errStr = "mismatched number of concrete terms in left- and right-hand side of:\n" ++ toSource conv}
   Just newmidd -> return R{leftneg = fmap no' left', middle = newmidd, rightneg = fmap no' right'}
 return(Env{pun=punct, bools=vars},rules)


 
zipEither :: [Either a b] -> [c] -> Maybe [Either a (b,c)]
zipEither [] [] = Just []
zipEither [] _ = Nothing
zipEither (Left a:xs) cs = (Left a :) <$> zipEither xs cs
zipEither (Right _:_) [] = Nothing
zipEither (Right b:xs) (c:cs) = (Right (b,c) :) <$> zipEither xs cs

resolveSelect :: M.Map Identifier (Choose Quote) -> Select -> Either SemanticError (Either Boundary_ (Choose String))
resolveSelect _ Boundary2 = return $ Left ()
resolveSelect _ (Pipe(Ch quotes)) = return $ (Right . Ch . map unQ) quotes
resolveSelect defs (Iden iden) = case iden `M.lookup` defs of
 Nothing -> Left E{errNum = 335, errStr = "unresolved identifier {" ++ unId iden ++ "}"}
 Just (Ch quotes) -> return $ (Right . Ch . map unQ) quotes

phonToW :: Phoneme -> W
phonToW Dollar = Dollar_
phonToW (Slash str) = W str


