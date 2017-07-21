{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

module Akrantiain.Sents_to_func
(sentsToFunc
,Input
,Output
) where
import Prelude hiding (undefined)
import Akrantiain.Structure
import Akrantiain.Errors
import Akrantiain.Rule
import Control.Monad(forM,when)
import Akrantiain.Pattern_match
import qualified Data.Map as M
import Data.Maybe(mapMaybe,maybeToList)
import Akrantiain.SanitizeSentences

type Input = String
type Output = Either RuntimeError String



sentsToFunc :: Set Sentence -> SemanticMsg (Input -> Output)
sentsToFunc sents = do
 SanitizeSentences(vars, convs, defs_) <- sanitizeSentences sents
 let punct = case Id "PUNCTUATION" `M.lookup` defs_ of{Nothing -> "";
  Just (Ch arr) -> arr >>= unQ} -- FIXME: THIS CONCAT ISN'T RIGHT (, though, at least it is explicitly explained in manual)
 rules' <- lift $ forM convs $ handleConv defs_
 let env' = Env{pun=punct, bools=vars}
 (env,rules) <- forbidExplicitSpacepunctMatching (env',rules')
 return $ cook (env,rules)




forbidExplicitSpacepunctMatching :: (Environment,[Rule]) -> SemanticMsg (Environment,[Rule])
forbidExplicitSpacepunctMatching (env,rules) = do
 let puncts = pun env
 let illegals = concatMap (searchPunct puncts) rules
 if null illegals
  then return (env,rules)
  else lift $ Left E{errNum=337, errStr = "a punctuation or space found inside a pattern string(s) "++toBraces (map Quote illegals)}

searchPunct :: Punctuation -> Rule -> [String]
searchPunct p R{leftneg =ln, leftdollar =ld, middle =m, rightdollar =rd, rightneg =rn} = 
 s3 ln ++ concatMap s2 ld ++ concatMap s1 m ++ concatMap s2 rd ++ s3 rn
 where
  s1 :: Foo -> [String]
  s1 (Left ()) = []
  s1 (Right (ch,_)) = s2 ch
  s2 :: Foo2 -> [String]
  s2 (Ch arr) = mapMaybe s5 arr
  s5 :: String -> Maybe String
  s5 str = if any (isSpPunct p . (:[])) str then Just str else Nothing
  s3 :: Maybe Condition -> [String]
  s3 mc = maybeToList mc >>= s4 where
   s4 NegBoundary = []
   s4 (Negation ch) = s2 ch



handleConv :: M.Map Identifier (Choose Quote) -> Conversion -> Either SemanticError Rule
handleConv defs_ conv@Conversion{lneg=left, mid=midd, rneg=right, phons=phonemes} = do
  let solve = resolveSelect defs_
  left'  <- traverse solve left
  right' <- traverse solve right
  midd'  <- traverse solve midd -- midd' :: [Either Boundary_ (Choose String)]
  when (all isDollar phonemes) $ Left E{errNum = 336, errStr = "right-hand side of the following sentence consists solely of dollar(s):\n" ++ toSource conv}
  case zipEither midd' (map phonToW phonemes) of
   Nothing -> Left E{errNum = 333, errStr = "mismatched number of concrete terms in left- and right-hand side of:\n" ++ toSource conv ++ "\nleft: " ++ show(length[()|Right _ <- midd']) ++ "; right: " ++ show(length phonemes)}
   Just newmidd -> do
    let (l_,mr_) = spanAndConvert toFoo2 newmidd
    let (m_,r_) = spanAndConvertRight toFoo2 mr_
    return R{leftneg = fmap no' left', leftdollar = l_, middle = m_, rightdollar= r_, rightneg = fmap no' right'}

spanAndConvert :: (a -> Maybe b) -> [a] -> ([b],[a])
spanAndConvert _ xs@[]            =  ([], xs)
spanAndConvert p xs@(x:xs') = case p x of
  Just y ->  let (ys,zs) = spanAndConvert p xs' in (y:ys,zs)
  Nothing -> ([],xs)


spanAndConvertRight :: (a -> Maybe b) -> [a] -> ([a], [b])
spanAndConvertRight f arr = let (a,b) = spanAndConvert f (reverse arr) in (reverse b, reverse a)



toFoo2 :: Foo -> Maybe Foo2
toFoo2 (Left ()) = Nothing
toFoo2 (Right (_,W _)) = Nothing
toFoo2 (Right (b,Dollar_)) = Just b

-- throw nothing if (# of Right in first arg) /= (# of second arg)
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

isDollar :: Phoneme -> Bool
isDollar Dollar = True
isDollar _ = False

phonToW :: Phoneme -> W
phonToW Dollar = Dollar_
phonToW (Slash str) = W str
