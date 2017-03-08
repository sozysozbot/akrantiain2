{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

import Akrantiain.Pattern_match
import System.IO
main :: IO ()
main = do
 cook (punct,rls) "sashimi"       >>>= putStrLn
 cook (punct,rls) "stoxiet"       >>>= putStrLn
 cook (punct,rls) "exiu"          >>>= putStrLn
 cook (punct,rls) "selxiunk"      >>>= putStrLn
 cook (punct,rls) "mi"            >>>= putStrLn
 cook (punct,rls) "liaxa"         >>>= putStrLn
 cook (punct,rls) "lineparine"    >>>= putStrLn
 cook (punct,rls) "krante"        >>>= putStrLn
 cook (punct,rls) "lkurftlesse'd" >>>= putStrLn
 cook (punct,rls) "xorlnemj"      >>>= putStrLn
 cook (punct,rls) "akrantiain"    >>>= putStrLn
 cook (punct,rls) "aus"           >>>= putStrLn
 cook (punct,rls) "panqa'dy"      >>>= putStrLn

(>>>=) :: (Show a) => Either a b -> ( b -> IO ()) -> IO ()
Left  a >>>= _  = hPutStrLn stderr $ show a
Right b >>>= f  = f b

c = Ch . (:[])

lift a = R{leftneg = Nothing, middle = a, rightneg = Nothing}

punct :: Punctuation
punct = ",.!?"

rls :: [Rule]
rls = [
 lift[Right(c"sh", W"ʃ")],
 lift[Right(c"x",Dollar_),Right(c"i", W""),Right(vowel,Dollar_)],
 (lift[Right(vowel,Dollar_),Right(c"i",W"j")]){rightneg =Just(no$c"r")},
 (lift[Right(vowel,Dollar_),Right(c"u",W"w")]){rightneg =Just(no$c"r")},
 lift[Right(c"i",W"j"),Right(vowel,Dollar_)],
 (lift[Right(c"s",W"s")]){rightneg = Just(no vowel)},
 (lift[Right(c"j",W"i")]){rightneg = Just(no vowel)},
 lift[Right(c"y",W"ɥ"),Left ()],
 (lift[Right(c"r",W"r")]){leftneg = Just (no vowel)},
 lift[Right(vowel,Dollar_), Right(c"r",W"ː")],
 lift[Right(c"t",W"t")],
 lift[Right(c"s",W"z")],
 lift[Right(c"y",W"y")],
 lift[Right(c"a",W"a")],
 lift[Right(c"i",W"i")],
 lift[Right(c"u",W"u")],
 lift[Right(c"m",W"m")],
 lift[Right(c"n",W"n")],
 lift[Right(c"k",W"k")],
 lift[Right(c"l",W"l")],
 lift[Right(c"s",W"s")],
 lift[Right(c"f",W"f")],
 lift[Right(c"x",W"ʃ")],
 lift[Right(c"e",W"e")],
 lift[Right(c"o",W"o")],
 lift[Right(c"d",W"d")],
 lift[Right(c"'",W "")],
 lift[Right(c"p",W"p")],
 lift[Right(c"q",W"kw")],
 lift[Right(c" ",W"")] -- need to be automatically inserted
 ]
--  


vowel :: Choose String
vowel = Ch ["a","e","i","o","u","y"]
 


