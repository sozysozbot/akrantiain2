{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

import Data.Maybe(fromJust, mapMaybe, isNothing)
import Data.List(isPrefixOf)
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

--  apply rule5 sashimi == [("s",Nothing),("a",Nothing),("s",Nothing),("h",Nothing),("i",Just "i"),("m",Nothing),("i",Just "i")]
--  apply rule2 sashimi == [("s",Nothing),("a",Nothing),("sh",Just "\643"),("i",Nothing),("m",Nothing),("i",Nothing)]
--  apply rule11 stoxiet == [("s",Just "s"),("t",Nothing),("o",Nothing),("x",Nothing),("i",Nothing),("e",Nothing),("t",Nothing)]
--  apply rule8 stoxiet == [("s",Nothing),("t",Nothing),("o",Nothing),("x",Nothing),("i",Just ""),("e",Nothing),("t",Nothing)]
--  apply rule1 sashimi == sashimi

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

--  match rule5 sashimi == [([("s",Nothing),("a",Nothing),("s",Nothing),("h",Nothing)],[("i",Just "i"),("m",Nothing),("i",Nothing)]),([("s",Nothing),("a",Nothing),("s",Nothing),("h",Nothing),("i",Nothing),("m",Nothing)],[("i",Just "i")])]
--  match rule2 sashimi == [([("s",Nothing),("a",Nothing)],[("sh",Just "\643"),("i",Nothing),("m",Nothing),("i",Nothing)])]
--  match rule11 stoxiet == [([],[("s",Just "s"),("t",Nothing),("o",Nothing),("x",Nothing),("i",Nothing),("e",Nothing),("t",Nothing)])]
--  match rule8 stoxiet == [([("s",Nothing),("t",Nothing),("o",Nothing)],[("x",Nothing),("i",Just ""),("e",Nothing),("t",Nothing)])]
--  match rule1 sashimi == []

match :: Rule -> Stat -> [(Front, Back)]
match [] stat = cutlist stat
match (Left condition :xs) stat = filter f $ match xs stat where
 f (front, _) = condition $ concat $ map fst front
match (Right(pat,w) :xs) stat = mapMaybe g $ match xs stat where
 g :: (Front, Back) -> Maybe (Front, Back)
 g (front, back) = do 
  let rev2 = map (\(a,b) -> (reverse a, b)) . reverse
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
rls = [rule2, rule8, rule11, rule1, rule3, rule4, rule5, rule6, rule7, rule9, rule10, rule12]
rule1, rule2, rule3, rule4, rule5, rule6, rule7, rule8, rule9, rule10, rule11, rule12 :: Rule

rule2 = [Right("sh", W"ʃ")]
rule8 = [Right("x",Dollar_), Right("i", W""), Right("e",Dollar_)]
rule11= [Right("s",W"s"), Left noVowel] 

rule1 = [Right("t", W"t")]
rule3 = [Right("s", W"z")]
rule4 = [Right("a",W"a")]
rule5 = [Right("i",W"i")]
rule6 = [Right("m",W"m")]
rule7 = [Right("s",W"s")]
rule9 = [Right("x", W"ʃ")]
rule10= [Right("e",W"e")]
rule12= [Right("o",W"o")]

noVowel :: Condition
noVowel str
 | null str = True
 | last str `elem` "aeiouy" = False
 | otherwise = True


