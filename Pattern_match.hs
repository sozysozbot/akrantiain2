import Data.Maybe(fromJust, mapMaybe)
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
cook' = undefined


-- merge is allowed, split is not
apply :: Rule -> Stat -> Stat
apply = undefined

-- cutlist [1,2,3] = [([],[1,2,3]),([1],[2,3]),([1,2],[3]),([1,2,3],[])]
cutlist :: [a] -> [([a],[a])]
cutlist [] = [([],[])]
cutlist a@(x:xs) =  ([],a): map f (cutlist xs) where f(a,b) = (x:a,b)



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
  return (rev2 $ drop(length taken)front', rev2 taken ++ back)
   
 
takeTill :: String -> [(String,a)] -> Maybe [(String, a)]
takeTill "" _ = Just []
takeTill str [] = Nothing
takeTill str (x@(s,a):xs)
 | s `isPrefixOf` str = (x:) <$> takeTill (drop(length s)str) xs
 | otherwise = Nothing






sample1 = "sashimi"
sample2 = "stoxiet"


rule1 = [Right("t", W"t")]
rule2 = [Right("sh", W"ʃ")]
rule3 = [Right("s", W"z")]
rule4 = [Right("a",W"a")]
rule5 = [Right("i",W"i")]
rule6 = [Right("m",W"m")]
rule7 = [Right("s",W"s")]
rule8 = [Right("x",Dollar_), Right("i", W""), Right("e",Dollar_)]
rule9 = [Right("x", W"ʃ")]
rule10= [Right("e",W"e")]
rule11= [Right("s",W"s"), Left noVowel] 
rule12= [Right("o",W"o")]

noVowel :: Condition
noVowel str
 | null str = True
 | last str `elem` "aeiouy" = False
 | otherwise = True


