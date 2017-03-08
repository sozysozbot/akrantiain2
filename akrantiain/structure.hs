{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

module Akrantiain.Structure
(Term(..)
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
,isConcreteTerm
,Select(..)
,Choose(..)
) where
import Prelude hiding (undefined)
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
 
 


newtype Options = F(Set Term) deriving(Show, Eq, Ord)
instance ToSource Options where
 toSource (F candids_set) = intercalate " | " (map toSource candids_set)


newtype Resolveds = R{ unR ::(Array Resolved) } deriving(Show, Eq, Ord)
newtype Term = C(Array PNCandidate) deriving(Show, Eq, Ord)
data Sentence = Conversion (Array Options) (Array Phoneme) | Define Identifier (Choose Quote) deriving(Show, Eq, Ord)
data PNCandidate = Neg Candidate | Pos Candidate deriving(Show, Eq, Ord)

data Candidate = Res Resolved | Ide Identifier deriving(Show, Eq, Ord)
data Resolved = Boundary | Quo Quote deriving(Show, Eq, Ord)



instance ToSource Resolved where
 toSource Boundary = "^"
 toSource (Quo quo) = toSource quo 



instance ToSource Candidate where
 toSource (Res res) = toSource res
 toSource (Ide ide) = toSource ide

instance ToSource Term where
 toSource (C arr) = intercalate " " (map toSource arr)

instance ToSource PNCandidate where
 toSource (Pos cand) = toSource cand
 toSource (Neg cand) = '!':toSource cand

instance ToSource Sentence where
 toSource (Conversion orthos phonemes) = intercalate " "(map toSource' orthos) ++ " -> " ++ intercalate " " (map toSource phonemes) ++ ";\n"
  where
   toSource' :: Options -> String
   toSource' (F [x]) = toSource x
   toSource' u = "(" ++ toSource u ++ ")"
 toSource (Define ide options) = toSource ide ++ " = " ++ toSource options ++ ";\n"

isConcreteTerm :: Term -> Bool
isConcreteTerm(C arr2) = any isConc arr2 where
 isConc :: PNCandidate -> Bool
 isConc(Neg _) = False
 isConc(Pos (Ide _)) = True 
 isConc(Pos (Res Boundary)) = False 
 isConc(Pos (Res (Quo _))) = True 
