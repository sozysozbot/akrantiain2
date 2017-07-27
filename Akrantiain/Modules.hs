{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

module Akrantiain.Modules
(Module_(..)
,Module,Module2
,ModuleName(..)
,InsideModule_(..)
,InsideModule
,ModChain
,Module2s(..)
) where
-- import Prelude hiding (undefined)
import Akrantiain.Structure
import Akrantiain.Rule
import Akrantiain.SanitizeSentences
import Data.Aeson
import Data.Text(pack)
import qualified Data.Set as S
import qualified Data.Map as M

type Module = Module_ [Sentence]

data Module_ a = Module {moduleName :: ModuleName, insideModule :: InsideModule_ a}
data ModuleName = Arrow {before :: Identifier, after :: Identifier} | ModuleName Identifier | HiddenModule deriving(Show, Eq, Ord)
data InsideModule_ a = Sents a | ModuleChain ModChain
type InsideModule = InsideModule_ [Sentence]
type ModChain = [ModuleName]
type Module2 = Module_ SanitizedSentences
type InsideModule2 = InsideModule_ SanitizedSentences

newtype Module2s = Module2s{unModule2s :: [Module2]}

instance ToSource ModuleName where
 toSource (ModuleName i) = toSource i
 toSource (Arrow bef aft) = "(" ++ toSource bef ++ " => " ++ toSource aft ++ ")"
 toSource HiddenModule = toSource (Id "_Main")

instance ToJSON Module2s where --  TypeSynonymInstances, FlexibleInstances
 toJSON ms = object [ pack(toStr modName) .= toJSON inside | Module modName inside <- unModule2s ms ] 

toStr :: ModuleName -> String
toStr HiddenModule =  "_Main"
toStr (ModuleName i) = unId i
toStr (Arrow i j) = unId i ++ "=>" ++ unId j
{-
instance ToJSON Module2 where -- TypeSynonymInstances, FlexibleInstances
 toJSON (Module modName inside) =
  object ["moduleName" .= modName, "content" .= inside]
-}
instance ToJSON ModuleName where
 toJSON mn = String . pack $ toStr mn 

instance ToJSON InsideModule2  where -- TypeSynonymInstances, FlexibleInstances
 toJSON (ModuleChain mods) = toJSON mods
 toJSON (Sents arr) = case arr of
  SanitizedSentences vars convs defs_ -> object [ "define" .= f defs_, "conversions" .= convs, "option" .= h vars]
  where  
   f :: M.Map Identifier (Choose Quote) -> Value
   f k = object . map (uncurry q) $ M.toList k    
   -- q :: Identifier -> Choose Quote -> Pair
    where q (Id i) (Ch qs) = pack i .= toJSON qs
   h :: S.Set SettingSpecifier -> Value
   h k = object . map r $ S.toList k
    where r setting = pack (toSource setting) .= toJSON True
