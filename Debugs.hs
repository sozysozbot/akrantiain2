{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Debugs
(Rule(..)
,Environment(..)
,Choose(..)
,Punctuation
,W(..)
,Condition(..)
,cook
,SettingSpecifier(..)

,makeEnv
,(>>>=)
,lift,lift2
,c
)where
import Prelude hiding (undefined)
import Akrantiain.Pattern_match
import Akrantiain.Structure
import Akrantiain.Rule
import System.IO
import qualified Data.Set as S


makeEnv :: Punctuation -> Environment
makeEnv punct = Env{pun=punct, bools=S.fromList[CASE_SENSITIVE]}

(>>>=) :: (Show a) => Either a b -> ( b -> IO ()) -> IO ()
Left  a >>>= _  = hPrint stderr a
Right b >>>= f  = f b

lift a = R{leftneg = Nothing, leftdollar = [], middle = a, rightdollar = [], rightneg = Nothing}
lift2 d a b = R{leftneg = Nothing, leftdollar = d, middle = a, rightdollar = b, rightneg = Nothing}

c :: a -> Choose a
c = Ch . (:[])

