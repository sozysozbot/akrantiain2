{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
import Prelude hiding (undefined)
import Debugs


main :: IO ()
main = do
 cook (env,rls) "a"       >>>= putStrLn
 cook (env,rls) "n"       >>>= putStrLn
 cook (env,rls) "an"      >>>= putStrLn
 cook (env,rls) "na"      >>>= putStrLn

env :: Environment
env = makeEnv punct


punct :: Punctuation
punct = ""

{-
"n" "a" "" -> $ $ //
"a" -> /a/; "n" -> //;
-}

rls :: [Rule]
rls = [
 lift2[c"n",c"a"][Right(c"",W"")][],
 lift[Right(c"a",W"a")],
 lift[Right(c"n",W"")]
 ]
