{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Akrantiain.Pattern_match
(Condition
,Rule
,Stat
,Front
,Back
,W(..)
,cook
) where
import Data.Maybe(mapMaybe, isNothing)
import Data.List(isPrefixOf, inits)
import Akrantiain.Errors
data W = W String | Dollar_ 

type Condition = (String -> Bool)
type Rule = [Either Condition (String, W)]

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
match [] stat = cutlist stat
match (Left condition :xs) stat = filter f $ match xs stat where
 f (_, back) = upgrade condition $ concat $ map fst back 
match (Right(pat,w) :xs) stat = mapMaybe g $ match xs stat where
 g :: (Front, Back) -> Maybe (Front, Back)
 g (front, back) = do 
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
