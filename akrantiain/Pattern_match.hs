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
) where
import Data.Maybe(mapMaybe, isNothing)
import Data.List(isPrefixOf, inits, tails)
import Data.Char(isSpace)
import Control.Monad(guard)
import Akrantiain.Errors

no :: Choose String -> Condition
no (Ch foo) str
 | null str = True
 | str `elem` foo = False
 | otherwise = True


data W = W String | Dollar_ 
data Choose a = Ch [a] deriving(Show, Eq, Ord)

type Boundary_ = ()
type Condition = (String -> Bool)
data Rule = R{leftneg :: Maybe(Condition), middle :: [ Either Boundary_ (Choose String, W)], rightneg :: Maybe(Condition)}

type Stat = [(String, Maybe String)]
type Front = [(String, Maybe String)]
type Back = [(String, Maybe String)]

nazo :: (String,Maybe b) -> Either RuntimeError b
nazo (_, Just b) = Right b
nazo (a, Nothing) = Left $ RE{errNo = 210, errMsg = "no rules that can handle character {" ++ a ++ "}"}

cook :: [Rule] -> String -> Either RuntimeError String
cook rls str = do 
 strs <- mapM nazo (cook' rls stat)
 return $ concat strs
 where 
  stat = map (\x -> ([x], Nothing)) (str ++ " ") -- extra space required for handling word boundary


cook' :: [Rule] -> Stat -> Stat
cook' rls stat = foldl (flip apply) stat rls

-- merge is allowed, split is not
apply :: Rule -> Stat -> Stat
apply rule stat = case match rule stat of 
 [] -> stat
 ((a,b):_) -> apply rule (a++b)

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

match :: Rule -> Stat -> [(Front, Back)]
match k@R{leftneg=Just condition} stat = filter f $ match k{leftneg=Nothing} stat where
 f (front, _) = upgrade2 condition $ concat $ map fst front
match R{middle =[], rightneg=Nothing} stat = cutlist stat
match R{middle=[], rightneg=Just condition} stat = filter f $ cutlist stat where
 f (_, back) = upgrade condition $ concat $ map fst back
match k@R{middle=Right(Ch pats,w):xs} stat = concatMap fff pats where 
 fff pat = mapMaybe (g pat) $ match k{middle=xs} stat
 g :: String -> (Front, Back) -> Maybe (Front, Back)
 g pat (front, back) = do 
  let front' = rev2 front
  let pat' = reverse pat
  taken <- takeTill pat' front'
  let taken' = rev2 taken
  case w of
   W w' -> if all (isNothing . snd) taken' then return (rev2 $ drop(length taken')front', (pat,Just w') : back) else Nothing
   Dollar_ -> return (rev2 $ drop(length taken')front', taken' ++ back)
match k@R{middle=Left():xs} stat = mapMaybe h $ match k{middle=xs} stat where
 h (front, back) = do
  let front' = reverse front
  guard $ null front || (isSpaces . fst . head) front
  let (b', f'') = span (isSpaces . fst) front'
  return (reverse f'', reverse b' ++ back)

isSpaces :: String -> Bool
isSpaces str = all isSpace str

takeTill :: String -> [(String,a)] -> Maybe [(String, a)]
takeTill "" _ = Just []
takeTill _ [] = Nothing
takeTill str (x@(s,_):xs)
 | s `isPrefixOf` str = (x:) <$> takeTill (drop(length s)str) xs
 | otherwise = Nothing
