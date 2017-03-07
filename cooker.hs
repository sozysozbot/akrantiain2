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
 lift[(c"sh", W"ʃ")],
 lift[(c"x",Dollar_),(c"i", W""), (vowel,Dollar_)],
 lift[(c"i",W"j"),(vowel,Dollar_)],
 (lift[(c"s",W"s")]){rightneg = Just(no vowel)},
 (lift[(c"r",W"r")]){leftneg = Just (no vowel)},
 lift[(vowel,Dollar_), (c"r",W"ː")],
 lift[(c"t", W"t")],
 lift[(c"s", W"z")],
 lift[(c"a",W"a")],
 lift[(c"i",W"i")],
 lift[(c"u",W"u")],
 lift[(c"m",W"m")],
 lift[(c"n",W"n")],
 lift[(c"k",W"k")],
 lift[(c"l",W"l")],
 lift[(c"s",W"s")],
 lift[(c"x",W"ʃ")],
 lift[(c"e",W"e")],
 lift[(c"o",W"o")],
 lift[(c"p",W"p")]
 ]
--  

vowel :: Choose String
vowel = Ch ["a","e","i","o","u","y"]
 


