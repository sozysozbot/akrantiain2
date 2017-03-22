{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
import Prelude hiding (undefined)
import Akrantiain.Pattern_match
import Akrantiain.Structure
import Akrantiain.Rule
import System.IO
import qualified Data.Map as M

env :: Environment
env = Env{pun=punct, bools=M.fromList[(Id"CASE_SENSITIVE",())]}

main :: IO ()
main = do
 cook (env,rls) "a"       >>>= putStrLn
 cook (env,rls) "n"       >>>= putStrLn
 cook (env,rls) "an"      >>>= putStrLn
 cook (env,rls) "na"      >>>= putStrLn

(>>>=) :: (Show a) => Either a b -> ( b -> IO ()) -> IO ()
Left  a >>>= _  = hPrint stderr a
Right b >>>= f  = f b

c :: a -> Choose a
c = Ch . (:[])

lift a = R{leftneg = Nothing, middle = a, rightneg = Nothing}

punct :: Punctuation
punct = ""

{-
"n" "a" "" -> $ $ //
"a" -> /a/; "n" -> //;
-}

rls :: [Rule]
rls = [
 lift[Right(c"n",Dollar_),Right(c"a",Dollar_),Right(c"",W"")],
 lift[Right(c"a",W"a")],
 lift[Right(c"n",W"")]
 ]



