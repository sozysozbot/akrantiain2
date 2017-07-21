{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}

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
,toBraces
) where
import Prelude hiding (undefined)
import Data.List(intercalate)
import Akrantiain.Global
import Data.Aeson hiding (Array)
import Data.Text(pack)
import Data.Maybe(maybeToList)

newtype Choose a = Ch{unCh::[a]} deriving(Show, Eq, Ord)
data Phoneme = Dollar | Slash String deriving(Show, Eq, Ord)
data Select = Boundary2 | Iden Identifier | Pipe (Choose Quote) deriving(Show, Eq, Ord)
newtype Quote = Quote{unQ::String} deriving(Show, Eq, Ord)
newtype Identifier = Id{unId::String} deriving(Show, Eq, Ord)
data Sentence = Left' Conversion | Middle' Identifier | Right' Define deriving(Show, Eq, Ord)
data Conversion = Conversion {mid::Array Select, phons:: Array Phoneme, lneg ::Maybe Select, rneg::Maybe Select} deriving(Show, Eq, Ord)
data Define = Define Identifier (Choose Quote) deriving(Show, Eq, Ord)

instance (Functor Choose) where
 fmap f (Ch list) = Ch(map f list)

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


toBraces :: ToSource a => [a] -> String
toBraces list = "{" ++ intercalate "}, {" (map toSource list)++ "}"


instance ToJSON Identifier where
 toJSON (Id i) = String (pack i)

instance ToJSON Quote where
 toJSON (Quote str) = toJSON str

-- data Conversion = Conversion {mid::Array Select, phons:: Array Phoneme, lneg ::Maybe Select, rneg::Maybe Select} deriving(Show, Eq, Ord)
instance ToJSON Conversion where
 toJSON Conversion{mid=selects, phons=phonemes, lneg=l, rneg=r} 
  = object [ "selects" .= arr1, "phonemes" .= toJSON phonemes] where
   arr1 = f l ++ map toJSON selects ++ f r `asTypeOf` [Null]
   f ma = maybeToList $ do
    a <- ma
    return $ object ["not" .= a]

instance ToJSON Phoneme where
 toJSON Dollar = Null 
 toJSON (Slash str) = toJSON str

instance ToJSON Select where
 toJSON Boundary2 = object ["bound" .= True]
 toJSON (Iden i) = object ["id" .= i]
 toJSON (Pipe (Ch qs)) = object ["or" .= toJSON qs]




