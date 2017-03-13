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
,Conversion(..)
,Define(..)
) where
import Prelude hiding (undefined)
import Data.List(intercalate)
import Akrantiain.Global

newtype Choose a = Ch [a] deriving(Show, Eq, Ord)
data Phoneme = Dollar | Slash String deriving(Show, Eq, Ord)
data Select = Boundary2 | Iden Identifier | Pipe (Choose Quote) deriving(Show, Eq, Ord)
newtype Quote = Quote{unQ::String} deriving(Show, Eq, Ord)
newtype Identifier = Id{unId::String} deriving(Show, Eq, Ord)
data Sentence = Left' Conversion | Middle' Identifier | Right' Define deriving(Show, Eq, Ord) 
data Conversion = Conversion {mid::Array Select, phons:: Array Phoneme, lneg ::Maybe Select, rneg::Maybe Select} deriving(Show, Eq, Ord) 
data Define = Define Identifier (Choose Quote) deriving(Show, Eq, Ord)


class ToSource a where
 toSource :: a -> String
instance ToSource Phoneme where
 toSource Dollar = "$"
 toSource (Slash str) = '/':str++"/"
instance ToSource Quote where
 toSource (Quote str) = '"':str++"\""
instance ToSource Identifier where
 toSource (Id str) = str
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
instance ToSource Conversion where
 toSource Conversion{mid=selects, phons=phonemes, lneg=left, rneg=right} = fromMaybe left ++ unwords(map toSource selects) ++ fromMaybe right ++ " -> " ++ unwords(map toSource phonemes) ++ ";\n"
  where
   fromMaybe :: (ToSource a) => Maybe a -> String
   fromMaybe (Just a) = "!" ++ toSource a
   fromMaybe Nothing = ""
instance ToSource Define where
 toSource (Define ide options) = toSource ide ++ " = " ++ toSource options ++ ";\n"
