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
import Data.Maybe


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
    where q (Id i) (Ch qs) = pack i .= toJSON qs
   h :: S.Set SettingSpecifier -> Value
   h k = object . map r $ S.toList k
    where r setting = pack (toSource setting) .= toJSON True

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

