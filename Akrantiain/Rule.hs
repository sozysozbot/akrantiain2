{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Akrantiain.Rule
(Rule(..)
,W(..)
,Boundary_
,Condition(..)
,Punctuation
,no
,Environment(..)
,Rules
,no'
,isSpPunct
,SettingSpecifier(..)
,Settings
,toSettingSpecifier
) where
import Prelude hiding (undefined)
import Akrantiain.Structure
import qualified Data.Map as M
import Data.Char(isSpace)

data Rule = R{leftneg :: Maybe Condition, middle :: [ Either Boundary_ (Choose String, W)], rightneg :: Maybe Condition} deriving (Show, Eq, Ord)
data W = W String | Dollar_  deriving (Show, Eq, Ord)
type Boundary_ = ()
data Condition = Negation (Choose String) | NegBoundary deriving (Show, Eq, Ord)
type Punctuation = [Char]
data Environment = Env{pun :: Punctuation, bools :: Settings} deriving (Show, Eq, Ord)
type Rules = (Environment,[Rule])

data SettingSpecifier = CASE_SENSITIVE | FALL_THROUGH deriving (Show, Eq, Ord)
type Settings = M.Map Identifier ()

toSettingSpecifier :: Identifier -> Maybe SettingSpecifier
toSettingSpecifier (Id "CASE_SENSITIVE") = Just CASE_SENSITIVE
toSettingSpecifier (Id "FALL_THROUGH") = Just FALL_THROUGH
toSettingSpecifier _ = Nothing

isSpPunct :: Punctuation -> String -> Bool
isSpPunct punct = all (\x -> isSpace x || x `elem` punct)

no :: Choose String -> (String -> Bool)
no (Ch foo) str
 | null str = True
 | str `elem` foo = False
 | otherwise = True

no' :: Either Boundary_ (Choose String) -> Condition
no' (Right c) = Negation c
no' (Left ()) = NegBoundary
