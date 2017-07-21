{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}

module Akrantiain.Modules
(Module_(..)
,Module
,ModuleName(..)
,InsideModule_(..)
,InsideModule
,ModChain
) where
-- import Prelude hiding (undefined)
import Akrantiain.Structure
import Akrantiain.Rule
import Akrantiain.SanitizeSentences
import Data.Aeson
import Data.Text(pack)
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad.Writer

type Module = Module_ [Sentence]

data Module_ a = Module {moduleName :: ModuleName, insideModule :: InsideModule_ a}
data ModuleName = Arrow {before :: Identifier, after :: Identifier} | ModuleName Identifier | HiddenModule deriving(Show, Eq, Ord)
data InsideModule_ a = Sents a | ModuleChain ModChain
type InsideModule = InsideModule_ [Sentence]
type ModChain = [ModuleName]

instance ToSource ModuleName where
 toSource (ModuleName i) = toSource i
 toSource (Arrow bef aft) = "(" ++ toSource bef ++ " => " ++ toSource aft ++ ")"
 toSource HiddenModule = toSource (Id "_Main")

instance (ToJSON a) => ToJSON (Module_ a) where
 toJSON (Module modName inside) =
  object ["moduleName" .= modName, "content" .= inside]

instance ToJSON ModuleName where
 toJSON HiddenModule = String "_Main"
 toJSON (ModuleName i) = toJSON i
 toJSON (Arrow i j) = toJSON [i,j]

instance (ToJSON a) => ToJSON (InsideModule_ a) where
 toJSON (ModuleChain mods) = toJSON mods
 toJSON (Sents arr) = case a of
  Left _ -> Null
  Right (SanitizedSentences vars convs defs_,_) -> object [ "define" .= f defs_, "conversions" .= convs, "option" .= h vars]
  where 
   a = undefined -- runWriterT $ sanitizeSentences arr -- 
   f :: M.Map Identifier (Choose Quote) -> Value
   f k = object . map (uncurry q) $ M.toList k    
   -- q :: Identifier -> Choose Quote -> Pair
    where q (Id i) (Ch qs) = pack i .= toJSON qs
   h :: S.Set SettingSpecifier -> Value
   h k = object . map r $ S.toList k
    where r setting = pack (toSource setting) .= toJSON True
