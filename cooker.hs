{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

import Akrantiain.Pattern_match
main :: IO ()
main = do
 putStrLn $ (\(Right x) -> x) $ cook rls "sashimi"
 putStrLn $ (\(Right x) -> x) $ cook rls "stoxiet"

c = Ch . (:[])
 
rls :: [Rule]
rls = [
 [Right(c"sh", W"ʃ")],
 [Right(c"x",Dollar_), Right(c"i", W""), Right(c"e",Dollar_)],
 [Right(c"s",W"s"), Left(no vowel)],
 [Right(c"t", W"t")],
 [Right(c"s", W"z")],
 [Right(c"a",W"a")],
 [Right(c"i",W"i")],
 [Right(c"m",W"m")],
 [Right(c"s",W"s")],
 [Right(c"x",W"ʃ")],
 [Right(c"e",W"e")],
 [Right(c"o",W"o")]
 ]

vowel :: Choose String
vowel = Ch ["a","e","i","o","u","y"]
 


