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
,Foo
,Foo2
,W2
,Identity(..)
) where
import Prelude hiding (undefined)
import Akrantiain.Structure
import qualified Data.Set as S
import Data.Char(isSpace,toUpper)
import Akrantiain.NFD

apply_nfds :: Rule -> Rule
apply_nfds R{leftneg=l, leftdollar=ld, middle=m, rightdollar=rd, rightneg=r} = R{leftneg=fmap f l, leftdollar=map h ld, middle=map (fmap g) m, rightdollar=map h rd, rightneg=fmap f r} 
 where
  f :: Condition -> Condition
  f NegBoundary = NegBoundary
  f (Negation cs) = Negation (h cs)
  g :: (Choose String, W) -> (Choose String, W)
  g (a,b) = (h a,b')
   where b' = case b of{Dollar_ -> Dollar_; W str -> W (nfd str);}
  h :: Choose String -> Choose String
  h = fmap nfd

type Foo = Either Boundary_ (Choose String, W)
type Foo2 = Choose String

newtype Identity a = Identity{runIdentity:: a}  deriving(Show,Eq,Ord)

instance Functor Identity where
 fmap f (Identity a) = Identity(f a)

data Rule = R{leftneg :: Maybe Condition, leftdollar :: [Foo2], middle :: [Foo], rightdollar :: [Foo2], rightneg :: Maybe Condition} deriving (Show, Eq, Ord)
data W = W String | Dollar_  deriving (Show, Eq, Ord)
type W2 = ()
type Boundary_ = ()
data Condition = Negation (Choose String) | NegBoundary deriving (Show, Eq, Ord)
type Punctuation = [Char]
data Environment = Env{pun :: Punctuation, bools :: Settings} deriving (Show, Eq, Ord)
type Rules = (Environment,[Rule])

data SettingSpecifier = CASE_SENSITIVE | FALL_THROUGH | USE_NFD | PRESERVE_CASE deriving (Show, Eq, Ord)
type Settings = S.Set SettingSpecifier

toSettingSpecifier :: Identifier -> Maybe SettingSpecifier
toSettingSpecifier (Id a)
 | map toUpper a == "CASE_SENSITIVE" = Just CASE_SENSITIVE
 | map toUpper a == "FALL_THROUGH" = Just FALL_THROUGH
 | map toUpper a == "FALL_THRU" = Just FALL_THROUGH
 | map toUpper a == "FALLTHROUGH" = Just FALL_THROUGH
 | map toUpper a == "FALLTHRU" = Just FALL_THROUGH
 | map toUpper a == "USE_NFD" = Just USE_NFD
 | map toUpper a == "PRESERVE_CASE" = Just PRESERVE_CASE
 | otherwise = Nothing

instance ToSource SettingSpecifier where
 toSource CASE_SENSITIVE = "CASE_SENSITIVE"
 toSource FALL_THROUGH =" FALL_THROUGH"
 toSource USE_NFD = "USE_NFD"
 toSource PRESERVE_CASE = "PRESERVE_CASE"

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
