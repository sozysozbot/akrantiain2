{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

module Akrantiain.Structure
(Candidates(..)
,Sentence(..)
,PNCandidate(..)
,Phoneme(..)
,Candidate(..)
,Resolved(..)
,Identifier(..)
,Quote(..)
,Resolveds(..)
,Set
,Array
,ToSource(..)
,Options(..)
) where
import Prelude hiding (undefined)
import Data.List(intercalate)

type Set a = [a]
type Array a = [a]


data Phoneme = Dollar Int | Slash String deriving(Show, Eq, Ord)


class ToSource a where
 toSource :: a -> String

instance ToSource Phoneme where
 toSource (Dollar i) = '$':show i
 toSource (Slash str) = '/':str++"/"


newtype Options = F(Set Candidates) deriving(Show, Eq, Ord)
instance ToSource Options where
 toSource (F candids_set) = intercalate " | " (map toSource candids_set)


newtype Resolveds = R{ unR ::(Array Resolved) } deriving(Show, Eq, Ord)
newtype Candidates = C(Array PNCandidate) deriving(Show, Eq, Ord)
data Sentence = Conversion (Array Options) (Array Phoneme) | Define Identifier Options deriving(Show, Eq, Ord)
data PNCandidate = Neg Candidate | Pos Candidate deriving(Show, Eq, Ord)

data Candidate = Res Resolved | Ide Identifier deriving(Show, Eq, Ord)
data Resolved = Boundary | Quo Quote deriving(Show, Eq, Ord)
newtype Identifier = Id String deriving(Show, Eq, Ord)
newtype Quote = Quote String deriving(Show, Eq, Ord)



instance ToSource Resolved where
 toSource Boundary = "^"
 toSource (Quo (Quote str)) = '"':str++"\""

instance ToSource Identifier where
 toSource (Id str) = str

instance ToSource Candidate where
 toSource (Res res) = toSource res
 toSource (Ide ide) = toSource ide

instance ToSource Candidates where
 toSource (C arr) = intercalate " " (map toSource arr)

instance ToSource PNCandidate where
 toSource (Pos cand) = toSource cand
 toSource (Neg cand) = '!':toSource cand

instance ToSource Sentence where
 toSource (Conversion orthos phonemes) = intercalate " "(map toSource' orthos) ++ " -> " ++ intercalate " " (map toSource phonemes) ++ ";"
  where
   toSource' :: Options -> String
   toSource' (F [x]) = toSource x
   toSource' u = "(" ++ toSource u ++ ")"
 toSource (Define ide options) = toSource ide ++ " = " ++ toSource options

