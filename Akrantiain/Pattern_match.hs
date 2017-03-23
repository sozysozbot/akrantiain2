{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Akrantiain.Pattern_match
(cook
) where
import Prelude hiding (undefined)
import Data.Maybe(mapMaybe, isNothing, maybeToList)
import Data.List(isPrefixOf, inits, tails, intercalate)
import Data.Char(isSpace, toLower)
import Data.Either(lefts, rights)
import Control.Monad(guard)
import Akrantiain.Errors
import Akrantiain.Rule
import Akrantiain.Structure(Choose(..),Identifier(..))
import qualified Data.Map as M
import Control.Arrow(first)
import Control.Monad.Reader


type Stat = [(String, Maybe String)]
type Front = [(String, Maybe String)]
type Back = [(String, Maybe String)]

resolvePunctuation :: Environment -> (String,Maybe String) -> Either String String
resolvePunctuation _ (_, Just b) = Right b
resolvePunctuation Env{pun=p} (a, Nothing)
 | isSpPunct p a = Right " "
 | otherwise = Left a

insensitive :: Rule -> Rule
insensitive R{leftneg=l, middle=m, rightneg=r} = R{leftneg=fmap f l, middle=map(first h<$>) m, rightneg=fmap f r} where
 f :: Condition -> Condition
 f (Negation c) = Negation $ h c
 f NegBoundary = NegBoundary
 h :: Choose String -> Choose String
 h (Ch arr) = Ch . map (map toLower) $ arr
-- R{leftneg :: Maybe(Condition), middle :: [ Either Boundary_ (Choose String, W)], rightneg :: Maybe(Condition)}


cook :: Rules -> String -> Either RuntimeError String
cook (env,rls') str = do
 let (rls,stat) = case M.lookup (Id "CASE_SENSITIVE") (bools env) of{
   Just () -> (rls', map (\x -> ([x], Nothing)) (str ++ " ")); -- extra space required for handling word boundary
   Nothing -> (map insensitive rls', map (\x -> ([toLower x], Nothing)) (str ++ " ")) }
 let cooked = cook' rls stat `runReader` env
 let eitherList = map (resolvePunctuation env) cooked
 case lefts eitherList of 
  [] -> return $ concat $ rights eitherList
  strs -> do 
   let msg = "{" ++ (intercalate "}, {") strs ++ "}" 
   Left RE{errNo = 210, errMsg = "no rules that can handle character(s) "++ msg} -- FIXME: better message that lets the user know which `r` made akrantiain crash


cook' :: [Rule] -> Stat -> Reader Environment Stat
cook' rls stat = foldM apply stat rls

-- merge is allowed, split is not
apply :: Stat -> Rule -> Reader Environment Stat
apply stat rule = do
 frontback_array <- match rule stat
 case frontback_array of 
  [] -> return stat
  c -> let (a,b) = last c in do
   newStat <- apply a rule
   return $ newStat ++ b 



 
-- cutlist [1,2,3] = [([],[1,2,3]),([1],[2,3]),([1,2],[3]),([1,2,3],[])]
cutlist :: [a] -> [([a],[a])]
cutlist [] = [([],[])]
cutlist u@(x:xs) =  ([],u): map f (cutlist xs) where f(a,b) = (x:a,b)



rev2 ::  [([a], t)] -> [([a], t)]
rev2 = map (first reverse) . reverse

upgrade :: ([a] -> Bool) -> ([a] -> Bool)
upgrade f str = all f $ inits str

upgrade2 :: ([a] -> Bool) -> ([a] -> Bool)
upgrade2 f str = all f $ tails str



match :: Rule -> Stat -> Reader Environment [(Front, Back)]

match R{leftneg=Nothing, middle =[], rightneg=Nothing} stat = return $ cutlist stat

match R{leftneg=Nothing, middle=[], rightneg=Just condition} stat = return $ filter f $ cutlist stat where
 f (_, back) = upgrade (unCond condition) $ concatMap fst back

match k@R{leftneg=Nothing, middle=Right(Ch pats,w):xs} stat =  do
 newMatch <- match k{middle=xs} stat 
 return $ foo pats w newMatch
match k@R{leftneg=Nothing, middle=Left():xs} stat = do 
 newMatch <- match k{middle=xs} stat
 env <- ask
 return $ mapMaybe (h env) newMatch where
 h env (front, back) = do
  let front' = reverse front
  let punct = pun env
  guard $ null front' || (isSpPunct punct . fst . head) front' -- FIXME: what if fst(head front') was an empty string?
  let (b', f'') = span (isSpPunct punct . fst) front'
  return (reverse f'', reverse b' ++ back)

match k@R{leftneg=Just condition} stat = do 
 newMatch <- match k{leftneg=Nothing} stat
 let f (front, _) = upgrade2 (unCond condition) $ concatMap fst front
 return $ filter f $ newMatch where



foo :: [String] -> W -> [(Front, Back)] -> [(Front, Back)]
foo pats w newMatch = do
 (front,back) <- newMatch 
 pat <- pats
 let front' = rev2 front
 let pat' = reverse pat
 taken <- maybeToList $ takeTill pat' front'
 let taken' = rev2 taken
 case w of
  W w' -> if all (isNothing . snd) taken' then return (rev2 $ drop(length taken')front', (pat,Just w') : back) else [] -- turn out not to be the cause
  Dollar_ -> return (rev2 $ drop(length taken')front', taken' ++ back)

isSpPunct :: Punctuation -> String -> Bool
isSpPunct punct = all (\x -> isSpace x || x `elem` punct)

takeTill :: String -> [(String,a)] -> Maybe [(String, a)]
takeTill "" _ = Just []
takeTill _ [] = Nothing
takeTill str (x@(s,_):xs)
 | s `isPrefixOf` str = (x:) <$> takeTill (drop(length s)str) xs
 | otherwise = Nothing
