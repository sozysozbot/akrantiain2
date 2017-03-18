{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Akrantiain.Resolve_modules
(modulesToFunc
) where
import Prelude hiding (undefined)
import Akrantiain.Modules
import Akrantiain.Structure
import Akrantiain.Sents_to_rules
import Akrantiain.Errors
import qualified Data.Map as M

data Module' = Module' {moduleName' :: ModuleName, insideModule' :: InsideModule'}
data InsideModule' = Func (Either SemanticError (Input -> Output)) | ModuleChain' [ModuleName]

newtype InsideModule3 = Funct (Either SemanticError (Input -> Output))
type Resmap = M.Map ModuleName InsideModule3

modulesToFunc :: Set Module -> Either SemanticError (Input -> Output)
modulesToFunc = modules'ToFunc . map moduleToModule'

-- return func from "_Main" module
modules'ToFunc :: Set Module' -> Either SemanticError (Input -> Output)
modules'ToFunc ms = do 
 resmap <- modules'ToResmap ms
 case ModuleName (Id "_Main") `M.lookup` resmap of
  Just (Funct func) -> func
  Nothing -> error "CANNOT HAPPEN" -- _Main always exists!

modules'ToResmap :: Set Module' -> Either SemanticError Resmap
modules'ToResmap [] = return M.empty
modules'ToResmap (Module'{moduleName' = name, insideModule' = Func func}:ms)
 = M.insert name (Funct func) <$> modules'ToResmap ms
 -- FIXME

liftLeft :: (a -> a) -> (Either a b -> Either a b)
liftLeft f (Left a) = Left $ f a
liftLeft _ x = x


moduleToModule' :: Module -> Module'
moduleToModule' Module {moduleName = name, insideModule = Sents sents}
 = Module'{moduleName' = name, insideModule' = Func (liftLeft f $ sentsToFunc sents)} where
  f :: SemanticError -> SemanticError
  f e = e{errStr = "Inside module "++ toSource name ++ ":\n"++ errStr e}  
moduleToModule' Module {moduleName = name, insideModule = ModuleChain chain}
 = Module'{moduleName' = name, insideModule' = ModuleChain' chain}

