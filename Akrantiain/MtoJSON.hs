{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
module Akrantiain.MtoJSON
(toJSON
) where
--import Prelude hiding (undefined)
import Akrantiain.Structure
import Akrantiain.Errors
import Akrantiain.Rule
import Control.Monad(forM,unless,when)
import Akrantiain.Pattern_match
import Akrantiain.Modules
import Akrantiain.SanitizeSentences
import Data.Aeson
import Data.Text(pack)
import Control.Monad.Writer

{-
data Module = Module {moduleName :: ModuleName, insideModule :: InsideModule}
data ModuleName = Arrow {before :: Identifier, after :: Identifier} | ModuleName Identifier | HiddenModule deriving(Show, Eq, Ord)
data InsideModule = Sents [Sentence] | ModuleChain ModChain
type ModChain = [ModuleName]
 
-}
instance ToJSON Module where
 toJSON (Module modName inside) =
  object ["moduleName" .= modName, "content" .= inside]

instance ToJSON ModuleName where
 toJSON HiddenModule = String "_Main"
 toJSON (ModuleName i) = toJSON i
 toJSON (Arrow i j) = toJSON [i,j]

instance ToJSON Identifier where
 toJSON (Id i) = String (pack i)

instance ToJSON InsideModule where
 toJSON (ModuleChain mods) = toJSON mods
 toJSON (Sents arr) = case a of
  Left _ -> Null
  Right ((vars,convs,defs_),_) -> object [ "define" .= f defs_, "conversions" .= g convs, "option" .= h vars]
  where 
   a = runWriterT $ sanitizeSentences arr -- (Either SemanticError) (a,[SemanticWarning])
   f k = undefined `asTypeOf` Null 
   g k = undefined `asTypeOf` Null
   h k = undefined `asTypeOf` Null

