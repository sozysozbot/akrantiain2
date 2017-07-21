{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
module Akrantiain.MtoJSON
(toJSON
) where
--import Prelude hiding (undefined)
import Akrantiain.Structure
import Akrantiain.Rule
import Akrantiain.Modules
import Akrantiain.SanitizeSentences
import qualified Data.Set as S
import qualified Data.Map as M
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
  Right ((vars,convs,defs_),_) -> object [ "define" .= f defs_, "conversions" .= convs, "option" .= h vars]
  where 
   -- a :: (Either SemanticError) (LONGTUPLE,[SemanticWarning])
   a = runWriterT $ sanitizeSentences arr -- 
   f :: M.Map Identifier (Choose Quote) -> Value
   f k = object . map (uncurry q) $ M.toList k    
   -- q :: Identifier -> Choose Quote -> Pair
   q (Id i) (Ch qs) = pack i .= toJSON qs

instance ToJSON Quote where
 toJSON (Quote str) = toJSON str

instance ToJSON Conversion where
 toJSON = undefined

h :: S.Set SettingSpecifier -> Value
h k = undefined `asTypeOf` Null

