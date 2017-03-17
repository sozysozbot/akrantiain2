{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Akrantiain.Resolve_modules
(modulesToFunc
) where
import Prelude hiding (undefined)
import Akrantiain.Modules
import Akrantiain.Structure
import Akrantiain.Sents_to_rules
import Akrantiain.Errors
{-
sentsToFunc :: Set Sentence -> Either SemanticError (Input -> Output)
-}

-- return func from "_Main" module
modulesToFunc :: Set Module -> Either SemanticError (Input -> Output)
modulesToFunc [Module(ModuleName (Id "_Main"))(Sents sents)] = sentsToFunc sents
 -- FIXME

liftLeft :: (a -> a) -> (Either a b -> Either a b)
liftLeft f (Left a) = Left $ f a
liftLeft f x = x


moduleToModule' :: Module -> Module'
moduleToModule' Module {moduleName = name, insideModule = Sents sents}
 = Module'{moduleName' = name, insideModule' = Func (liftLeft f $ sentsToFunc sents)} where
  f :: SemanticError -> SemanticError
  f e = e{errStr = "Inside module "++ toSource name ++ ":\n"++ errStr e}  
moduleToModule' Module {moduleName = name, insideModule = ModuleChain chain}
 = Module'{moduleName' = name, insideModule' = ModuleChain' chain}

 
data Module' = Module' {moduleName' :: ModuleName, insideModule' :: InsideModule'}
data InsideModule' = Func (Either SemanticError (Input -> Output)) | ModuleChain' [ModuleName]
