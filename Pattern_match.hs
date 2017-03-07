{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

import Data.Maybe(fromJust, mapMaybe, isNothing)
import Data.List(isPrefixOf, inits)
data W = W String | Dollar_ 

type Condition = (String -> Bool)
type Rule = [Either Condition (String, W)]

type Stat = [(String, Maybe String)]
type Front = [(String, Maybe String)]
type Back = [(String, Maybe String)]

cook :: String -> String
cook str = concat $ map (fromJust . snd) $ cook' $ map (\x -> ([x], Nothing)) str

cook' :: Stat -> Stat
cook' stat = foldl (flip apply) stat rls

-- merge is allowed, split is not
apply :: Rule -> Stat -> Stat
apply rule stat = case match rule stat of 
 [] -> stat
 ((a,b):_) -> apply rule (a++b)

-- cutlist [1,2,3] = [([],[1,2,3]),([1],[2,3]),([1,2],[3]),([1,2,3],[])]
cutlist :: [a] -> [([a],[a])]
cutlist [] = [([],[])]
cutlist u@(x:xs) =  ([],u): map f (cutlist xs) where f(a,b) = (x:a,b)

sashimi, stoxiet :: Stat
sashimi = [("s",Nothing),("a",Nothing),("s",Nothing),("h",Nothing),("i",Nothing),("m",Nothing),("i",Nothing)]
stoxiet = [("s",Nothing),("t",Nothing),("o",Nothing),("x",Nothing),("i",Nothing),("e",Nothing),("t",Nothing)]

rev2 ::  [([a], t)] -> [([a], t)]
rev2 = map (\(a,b) -> (reverse a, b)) . reverse


main = do
 putStrLn $ cook "sashimi"
 putStrLn $ cook "stoxiets"

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

rls :: [Rule]
rls = [
 [Right("sh", W"ʃ")],
 [Right("x",Dollar_), Right("i", W""), Right("e",Dollar_)],
 [Right("s",W"s"), Left noVowel'],
 [Right("t", W"t")],
 [Right("s", W"z")],
 [Right("a",W"a")],
 [Right("i",W"i")],
 [Right("m",W"m")],
 [Right("s",W"s")],
 [Right("x",W"ʃ")],
 [Right("e",W"e")],
 [Right("o",W"o")]
 ]


 
noVowel' :: Condition
noVowel' str
 | null str = True
 | str `elem` ["a","e","i","o","u","y"] = False
 | otherwise = True


upgrade :: ([a] -> Bool) -> ([a] -> Bool)
upgrade f str = all f $ inits str
