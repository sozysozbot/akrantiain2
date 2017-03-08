{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

module Akrantiain.Structure
(Sentence(..)
,Phoneme(..)
,Identifier(..)
,Quote(..)
,Set
,Array
,ToSource(..)
,Select(..)
,Choose(..)
) where
-- import Prelude hiding (undefined)
import Data.List(intercalate)
import Akrantiain.Global

data Choose a = Ch [a] deriving(Show, Eq, Ord)

data Phoneme = Dollar | Slash String deriving(Show, Eq, Ord)

instance ToSource Phoneme where
 toSource (Dollar) = "$"
 toSource (Slash str) = '/':str++"/"

data Select = Boundary2 | Iden Identifier | Pipe (Choose Quote) deriving(Show, Eq, Ord)
newtype Quote = Quote String deriving(Show, Eq, Ord)
newtype Identifier = Id String deriving(Show, Eq, Ord)

instance ToSource Quote where
 toSource (Quote str) = '"':str++"\""
instance ToSource Identifier where
 toSource (Id str) = str
class ToSource a where
 toSource :: a -> String

instance ToSource Select where
 toSource Boundary2 = "^"
 toSource(Iden i) = toSource i
 toSource(Pipe(Ch arr)) = case arr of
  [x] -> toSource x
  _ -> "(" ++ intercalate " | " (map toSource arr) ++ ")"

instance ToSource a => ToSource (Choose a) where
 toSource (Ch arr) = case arr of
  [x] -> toSource x
  _ -> "(" ++ intercalate " | " (map toSource arr) ++ ")"
 
 
data Sentence = Conversion {middle::(Array Select), phons:: (Array Phoneme), lneg ::Maybe Select, rneg::Maybe Select} | Define Identifier (Choose Quote) deriving(Show, Eq, Ord)


instance ToSource Sentence where
 toSource (Conversion selects phonemes left right) = fromMaybe left ++ intercalate " "(map toSource selects) ++ fromMaybe right ++ " -> " ++ intercalate " " (map toSource phonemes) ++ ";\n"
  where
   fromMaybe :: (ToSource a) => Maybe a -> String
   fromMaybe (Just a) = "!" ++ toSource a
   fromMaybe Nothing = ""
 toSource (Define ide options) = toSource ide ++ " = " ++ toSource options ++ ";\n"


