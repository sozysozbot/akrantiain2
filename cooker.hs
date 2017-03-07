{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

import Akrantiain.Pattern_match
main :: IO ()
main = do
 putStrLn $ (\(Right x) -> x) $ cook rls "sashimi"
 putStrLn $ (\(Right x) -> x) $ cook rls "stoxiet"

 
rls :: [Rule]
rls = [
 [Right("sh", W"ʃ")],
 [Right("x",Dollar_), Right("i", W""), Right("e",Dollar_)],
 [Right("s",W"s"), Left(no vowel)],
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

vowel :: Choose String
vowel = Ch ["a","e","i","o","u","y"]
 


