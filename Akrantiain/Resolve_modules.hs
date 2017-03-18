{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Akrantiain.Resolve_modules
(modulesToFunc
) where
import Prelude hiding (undefined)
import Akrantiain.Modules
import Akrantiain.Structure
import Akrantiain.Sents_to_rules
import Akrantiain.Errors

data Module' = Module' {moduleName' :: ModuleName, insideModule' :: InsideModule'}
data InsideModule' = Func (Either SemanticError (Input -> Output)) | ModuleChain' [ModuleName]


modulesToFunc :: Set Module -> Either SemanticError (Input -> Output)
modulesToFunc = modules'ToFunc . map moduleToModule'

-- return func from "_Main" module
modules'ToFunc :: Set Module' -> Either SemanticError (Input -> Output)
modules'ToFunc [Module'(ModuleName (Id "_Main"))(Func func)] = func
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

