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
rls = [
 lift[Right(c"sh", W"ʃ")],
 lift[Right(c"x",Dollar_), Right(c"i", W""), Right(vowel,Dollar_)],
 lift[Right(c"i",W"j"),Right(vowel,Dollar_)],
 (lift[Right(c"s",W"s")]){rightneg = Just(no vowel)},
 lift[Right(vowel,Dollar_), Right(c"r",W"ː")],
 lift[Right(c"t", W"t")],
 lift[Right(c"s", W"z")],
 lift[Right(c"a",W"a")],
 lift[Right(c"i",W"i")],
 lift[Right(c"u",W"u")],
 lift[Right(c"m",W"m")],
 lift[Right(c"n",W"n")],
 lift[Right(c"k",W"k")],
 lift[Right(c"l",W"l")],
 lift[Right(c"s",W"s")],
 lift[Right(c"x",W"ʃ")],
 lift[Right(c"e",W"e")],
 lift[Right(c"o",W"o")],
 lift[Right(c"p",W"p")]
 ]
--  lift[Left(no vowel), Right(c"r",W"r")],

vowel :: Choose String
vowel = Ch ["a","e","i","o","u","y"]
 


