{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

import Akrantiain.Pattern_match
main :: IO ()
main = do
 putStrLn $ (\(Right x) -> x) $ cook rls "sashimi"
 putStrLn $ (\(Right x) -> x) $ cook rls "stoxiet"
 putStrLn $ (\(Right x) -> x) $ cook rls "exiu"
 putStrLn $ (\(Right x) -> x) $ cook rls "selxiunk"
 putStrLn $ (\(Right x) -> x) $ cook rls "mi"
 putStrLn $ (\(Right x) -> x) $ cook rls "liaxa"
 putStrLn $ (\(Right x) -> x) $ cook rls "lineparine"

c = Ch . (:[])

lift a = R{leftneg = Nothing, middle = a, rightneg = Nothing}
 
rls :: [Rule]
rls = map lift [
 [Right(c"sh", W"ʃ")],
 [Right(c"x",Dollar_), Right(c"i", W""), Right(vowel,Dollar_)],
 [Right(c"i",W"j"),Right(vowel,Dollar_)],
 [Right(c"s",W"s"), Left(no vowel)],
 [Left(no vowel), Right(c"r",W"r")],
 [Right(vowel,Dollar_), Right(c"r",W"ː")],
 [Right(c"t", W"t")],
 [Right(c"s", W"z")],
 [Right(c"a",W"a")],
 [Right(c"i",W"i")],
 [Right(c"u",W"u")],
 [Right(c"m",W"m")],
 [Right(c"n",W"n")],
 [Right(c"k",W"k")],
 [Right(c"l",W"l")],
 [Right(c"s",W"s")],
 [Right(c"x",W"ʃ")],
 [Right(c"e",W"e")],
 [Right(c"o",W"o")],
 [Right(c"p",W"p")]
 ]

vowel :: Choose String
vowel = Ch ["a","e","i","o","u","y"]
 


