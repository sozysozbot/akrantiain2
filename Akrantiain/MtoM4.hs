{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Akrantiain.MtoM4
(moduleToModule4
,moduleToModule2
,Module2
,Module4
,InsideModule4
) where
import Akrantiain.Modules
import Akrantiain.Sents_to_func
import Akrantiain.Errors
import Akrantiain.Structure
import Control.Monad.Writer
import Akrantiain.SanitizeSentences
type Module4 = Module_ (Input -> Output)
-- data Module4 = Module4 {moduleName4 :: ModuleName, insideModule4 :: InsideModule4}
type InsideModule4 = InsideModule_ (Input -> Output) 


liftLeft :: (a -> c) -> (Either a b -> Either c b)
liftLeft f (Left a) = Left $ f a
liftLeft _ (Right x) = Right x

liftLeft2 :: (a -> c) -> (WriterT d (Either a) b -> WriterT d (Either c) b)
liftLeft2 f = WriterT . liftLeft f . runWriterT

mToM :: (a -> SemanticMsg b) -> Module_ a -> SemanticMsg (Module_ b)
mToM fun Module{moduleName = name, insideModule = Sents sents} = do
 new <- liftLeft2 f $ fun sents
 return Module{moduleName = name, insideModule = Sents new} where
  f :: SemanticError -> SemanticError
  f e = e{errStr = "Inside module "++ toSource name ++ ":\n"++ errStr e}
mToM _ Module {moduleName = name, insideModule = ModuleChain chain}
 = return Module{moduleName = name, insideModule = ModuleChain chain}

moduleToModule2 :: Module -> SemanticMsg Module2
moduleToModule2 = mToM sanitizeSentences

moduleToModule4 :: Module -> SemanticMsg Module4
moduleToModule4 = mToM sentsToFunc 
