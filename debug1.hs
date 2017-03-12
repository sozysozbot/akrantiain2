{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
import Prelude hiding (undefined)
import Akrantiain.Pattern_match
import System.IO
import qualified Data.Map as M

env :: Environment
env = Env{pun=punct, bools=M.empty}

main :: IO ()
main = do
 cook (env,rls) "sashimi"       >>>= putStrLn
 cook (env,rls) "stoxiet"       >>>= putStrLn
 cook (env,rls) "exiu"          >>>= putStrLn
 cook (env,rls) "selxiunk"      >>>= putStrLn
 cook (env,rls) "mi"            >>>= putStrLn
 cook (env,rls) "liaxa"         >>>= putStrLn
 cook (env,rls) "lineparine"    >>>= putStrLn
 cook (env,rls) "krante"        >>>= putStrLn
 cook (env,rls) "lkurftlesse'd" >>>= putStrLn
 cook (env,rls) "xorlnemj"      >>>= putStrLn
 cook (env,rls) "ayplerde"      >>>= putStrLn
 cook (env,rls) "akrantiain"    >>>= putStrLn
 cook (env,rls) "aus"           >>>= putStrLn
 cook (env,rls) "panqa'dy"      >>>= putStrLn
 cook (env,rls) "Fankaon kaccaon lex ta safes elx wioll ycax elx pojiv Zarhalo gasluifesj farkzirVion befivagRi'i qacemal xadlumirfa mol niv."      >>>= putStrLn

(>>>=) :: (Show a) => Either a b -> ( b -> IO ()) -> IO ()
Left  a >>>= _  = hPutStrLn stderr $ show a
Right b >>>= f  = f b

c = Ch . (:[])

lift a = R{leftneg = Nothing, middle = a, rightneg = Nothing}

punct :: Punctuation
punct = ",.!?"

palat :: Choose String
palat = Ch["z","x","ch","sh"]

rls :: [Rule]
rls = [
 lift[Left(), Right(c"wioll", W"wjol"), Left ()],
 lift[Right(c"sh", W"ʃ")],
 lift[Right(palat,Dollar_),Right(c"i", W""),Right(vowel,Dollar_)],
 (lift[Right(vowel,Dollar_),Right(c"i",W"j")]){rightneg =Just(Negation$c"r")},
 lift[Right(palat,Dollar_),Right(c"i",Dollar_),Right(c"u",W"u")],
 (lift[Right(vowel,Dollar_),Right(c"u",W"w")]){rightneg =Just(Negation$c"r")},
 (lift[Right(c"s",W"z"),Right(c"j",Dollar_)]){rightneg = Just(Negation vowel)},
 lift[Right(c"i",W"j"),Right(vowel,Dollar_)],
 (lift[Right(c"s",W"s")]){rightneg = Just(Negation vowel)},
 (lift[Right(c"j",W"i")]){rightneg = Just(Negation vowel)},
 (lift[Right(vowel,Dollar_),Right(c"y",W"ɥ")]){rightneg = Just(Negation vowel)},
 (lift[Right(Ch["r","R"],W"r")]){leftneg = Just (Negation vowel)},
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
 lift[Right(c"z",W"t͡s")],
 lift[Right(c"f",W"f")],
 lift[Right(c"F",W"ɸ")],
 lift[Right(c"V",W"β")],
 lift[Right(c"Z",W"d͡ʑ")],
 lift[Right(c"x",W"ʃ")],
 lift[Right(c"c",W"s")],
 lift[Right(c"j",W"j")],
 lift[Right(c"w",W"w")],
 lift[Right(c"e",W"e")],
 lift[Right(c"b",W"b")],
 lift[Right(c"g",W"g")],
 lift[Right(c"v",W"v")],
 lift[Right(c"h",W"h")],
 lift[Right(c"o",W"o")],
 lift[Right(c"d",W"d")],
 lift[Right(c"'",W "")],
 lift[Right(c"p",W"p")],
 lift[Right(c"q",W"kw")]
 ]


vowel :: Choose String
vowel = Ch ["a","e","i","o","u","y"]
 


