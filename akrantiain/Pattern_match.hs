{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Akrantiain.Pattern_match
(Condition
,Rule
,Stat
,Front
,Back
,W(..)
,cook
,Choose(..)
,no
,Rule(..)
) where
import Data.Maybe(mapMaybe, isNothing)
import Data.List(isPrefixOf, inits)
import Akrantiain.Errors

no :: Choose String -> Condition
no (Ch foo) str
 | null str = True
 | str `elem` foo = False
 | otherwise = True


data W = W String | Dollar_ 
data Choose a = Ch [a] deriving(Show, Eq, Ord)


type Condition = (String -> Bool)
data Rule = R{leftneg :: Maybe(Choose String), middle :: [ (Choose String, W)], rightneg :: Maybe(Condition)}

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
  stat = map (\x -> ([x], Nothing)) str


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

match :: Rule -> Stat -> [(Front, Back)]
match R{middle =[], rightneg=Nothing} stat = cutlist stat
match k@R{middle=[], rightneg=Just condition} stat = mapMaybe f $ cutlist stat where
 f a@(front, back)
  | upgrade condition $ concat $ map fst back = Just a
  | otherwise = Nothing
match k@R{middle=(((Ch pats),w) :xs)} stat = concatMap fff pats where 
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
 
takeTill :: String -> [(String,a)] -> Maybe [(String, a)]
takeTill "" _ = Just []
takeTill _ [] = Nothing
takeTill str (x@(s,_):xs)
 | s `isPrefixOf` str = (x:) <$> takeTill (drop(length s)str) xs
 | otherwise = Nothing


upgrade :: ([a] -> Bool) -> ([a] -> Bool)
upgrade f str = all f $ inits str
