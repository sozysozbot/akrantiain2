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
,apply_nfds
) where
import Prelude hiding (undefined)
import Akrantiain.Structure
import qualified Data.Set as S
import Data.Char(isSpace)

apply_nfds :: Rule -> Rule
apply_nfds R{leftneg=l, middle=m, rightneg=r} = R{leftneg=fmap f l, middle=map (fmap g) m, rightneg=fmap f r} 
 where
  f :: Condition -> Condition
  f = id -- FIXME
  g :: (Choose String, W) -> (Choose String, W)
  g = id -- FIXME

data Rule = R{leftneg :: Maybe Condition, middle :: [ Either Boundary_ (Choose String, W)], rightneg :: Maybe Condition} deriving (Show, Eq, Ord)
data W = W String | Dollar_  deriving (Show, Eq, Ord)
type Boundary_ = ()
data Condition = Negation (Choose String) | NegBoundary deriving (Show, Eq, Ord)
type Punctuation = [Char]
data Environment = Env{pun :: Punctuation, bools :: Settings} deriving (Show, Eq, Ord)
type Rules = (Environment,[Rule])

data SettingSpecifier = CASE_SENSITIVE | FALL_THROUGH | USE_NFD deriving (Show, Eq, Ord)
type Settings = S.Set SettingSpecifier

toSettingSpecifier :: Identifier -> Maybe SettingSpecifier
toSettingSpecifier (Id "CASE_SENSITIVE") = Just CASE_SENSITIVE
toSettingSpecifier (Id "FALL_THROUGH") = Just FALL_THROUGH
toSettingSpecifier (Id "USE_NFD") = Just USE_NFD
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
