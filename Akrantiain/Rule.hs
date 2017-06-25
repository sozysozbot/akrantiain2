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
import Data.Char(isSpace)
import Akrantiain.NFD

apply_nfds :: Rule -> Rule
apply_nfds R{leftneg=l, leftdollar=ld, middle=m, rightdollar=rd, rightneg=r} = R{leftneg=fmap f l, leftdollar=map (fmap g2) ld, middle=map (fmap g) m, rightdollar=map (fmap g2) rd, rightneg=fmap f r} 
 where
  f :: Condition -> Condition
  f NegBoundary = NegBoundary
  f (Negation cs) = Negation (h cs)
  g :: (Choose String, W) -> (Choose String, W)
  g (a,b) = (h a,b')
   where b' = case b of{Dollar_ -> Dollar_; W str -> W (nfd str);}
  g2 :: Choose String -> Choose String
  g2  = h
  h :: Choose String -> Choose String
  h = fmap nfd

type Foo = Either Boundary_ (Choose String, W)
type Foo2 = Identity(Choose String)

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
toSettingSpecifier (Id "CASE_SENSITIVE") = Just CASE_SENSITIVE
toSettingSpecifier (Id "FALL_THROUGH") = Just FALL_THROUGH
toSettingSpecifier (Id "USE_NFD") = Just USE_NFD
toSettingSpecifier (Id "PRESERVE_CASE") = Just PRESERVE_CASE
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
