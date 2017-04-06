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
data Environment = Env{pun :: Punctuation, bools :: M.Map Identifier ()} deriving (Show, Eq, Ord)
type Rules = (Environment,[Rule])


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
