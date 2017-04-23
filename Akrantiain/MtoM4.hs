{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Akrantiain.MtoM4
(moduleToModule4
,Module4(..)
,InsideModule4(..)
) where
import Akrantiain.Modules
import Akrantiain.Sents_to_rules
import Akrantiain.Errors
import Akrantiain.Structure

data Module4 = Module4 {moduleName4 :: ModuleName, insideModule4 :: InsideModule4}
data InsideModule4 = Func4 (Input -> Output) | ModuleChain4 [ModuleName]

liftLeft :: (a -> c) -> (Either a b -> Either c b)
liftLeft f (Left a) = Left $ f a
liftLeft _ (Right x) = Right x

moduleToModule4 :: Module -> SemanticMsg Module4
moduleToModule4 Module {moduleName = name, insideModule = Sents sents}
 = lift $ do
 func <- liftLeft f $ sentsToFunc sents
 return Module4{moduleName4 = name, insideModule4 = Func4 func} where
  f :: SemanticError -> SemanticError
  f e = e{errStr = "Inside module "++ toSource name ++ ":\n"++ errStr e}
moduleToModule4 Module {moduleName = name, insideModule = ModuleChain chain}
 = return Module4{moduleName4 = name, insideModule4 = ModuleChain4 chain}
