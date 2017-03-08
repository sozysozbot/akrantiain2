{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Akrantiain.Pattern_match
(Condition
,Stat
,Front
,Back
,W(..)
,cook
,Choose(..)
,no
,Rule(..)
,Boundary_
,Punctuation
) where
import Data.Maybe(mapMaybe, isNothing)
import Data.List(isPrefixOf, inits, tails, intersperse)
import Data.Char(isSpace)
import Data.Either(lefts, rights)
import Control.Monad(guard)
import Akrantiain.Errors
import qualified Data.Set as S

no :: Choose String -> Condition
no (Ch foo) str
 | null str = True
 | str `elem` foo = False
 | otherwise = True


data W = W String | Dollar_ 
data Choose a = Ch [a] deriving(Show, Eq, Ord)
type Punctuation = [Char]

type Boundary_ = ()
type Condition = (String -> Bool)
data Rule = R{leftneg :: Maybe(Condition), middle :: [ Either Boundary_ (Choose String, W)], rightneg :: Maybe(Condition)}

type Stat = [(String, Maybe String)]
type Front = [(String, Maybe String)]
type Back = [(String, Maybe String)]

nazo2 :: Punctuation -> (String,Maybe String) -> Either String String
nazo2 _ (_, Just b) = Right b
nazo2 p (a, Nothing)
 | isSpPunct p a = Right " "
 | otherwise = Left $ a


type Rules = (Punctuation,[Rule])

cook :: Rules -> String -> Either RuntimeError String
cook r@(punct,_) str = do 
 let eitherList = map (nazo2 punct) (cook' r stat)
 case lefts eitherList of 
  [] -> return $ concat $ rights eitherList
  strs -> do 
   let msg = "{" ++ (concat . intersperse "}, {"  . S.toList . S.fromList) strs ++ "}" 
   Left $ RE{errNo = 210, errMsg = "no rules that can handle character(s) "++ msg}
 where 
  stat = map (\x -> ([x], Nothing)) (str ++ " ") -- extra space required for handling word boundary


cook' :: Rules -> Stat -> Stat
cook' (punct,rls) stat = foldl (apply punct) stat rls

-- merge is allowed, split is not
apply :: Punctuation -> Stat -> Rule -> Stat
apply punct stat rule = case match punct rule stat of 
 [] -> stat
 ((a,b):_) -> apply punct (a++b) rule 

-- cutlist [1,2,3] = [([],[1,2,3]),([1],[2,3]),([1,2],[3]),([1,2,3],[])]
cutlist :: [a] -> [([a],[a])]
cutlist [] = [([],[])]
cutlist u@(x:xs) =  ([],u): map f (cutlist xs) where f(a,b) = (x:a,b)



rev2 ::  [([a], t)] -> [([a], t)]
rev2 = map (\(a,b) -> (reverse a, b)) . reverse

upgrade :: ([a] -> Bool) -> ([a] -> Bool)
upgrade f str = all f $ inits str

upgrade2 :: ([a] -> Bool) -> ([a] -> Bool)
upgrade2 f str = all f $ tails str

match :: Punctuation -> Rule -> Stat -> [(Front, Back)]
match punct k@R{leftneg=Just condition} stat = filter f $ match punct k{leftneg=Nothing} stat where
 f (front, _) = upgrade2 condition $ concat $ map fst front
match _ R{middle =[], rightneg=Nothing} stat = cutlist stat
match _ R{middle=[], rightneg=Just condition} stat = filter f $ cutlist stat where
 f (_, back) = upgrade condition $ concat $ map fst back
match punct k@R{middle=Right(Ch pats,w):xs} stat = concatMap fff pats where 
 fff pat = mapMaybe (g pat) $ match punct k{middle=xs} stat
 g :: String -> (Front, Back) -> Maybe (Front, Back)
 g pat (front, back) = do 
  let front' = rev2 front
  let pat' = reverse pat
  taken <- takeTill pat' front'
  let taken' = rev2 taken
  case w of
   W w' -> if all (isNothing . snd) taken' then return (rev2 $ drop(length taken')front', (pat,Just w') : back) else Nothing
   Dollar_ -> return (rev2 $ drop(length taken')front', taken' ++ back)
match punct k@R{middle=Left():xs} stat = mapMaybe h $ match punct k{middle=xs} stat where
 h (front, back) = do
  let front' = reverse front
  guard $ null front' || (isSpPunct punct . fst . head) front'
  let (b', f'') = span (isSpPunct punct . fst) front'
  return (reverse f'', reverse b' ++ back)

isSpPunct :: Punctuation -> String -> Bool
isSpPunct punct str = all (\x -> isSpace x || x `elem` punct) str

takeTill :: String -> [(String,a)] -> Maybe [(String, a)]
takeTill "" _ = Just []
takeTill _ [] = Nothing
takeTill str (x@(s,_):xs)
 | s `isPrefixOf` str = (x:) <$> takeTill (drop(length s)str) xs
 | otherwise = Nothing
