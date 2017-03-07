{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

import Akrantiain.Pattern_match
main = do
 putStrLn $ cook rls "sashimi"
 putStrLn $ cook rls "stoxiet"

 
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

