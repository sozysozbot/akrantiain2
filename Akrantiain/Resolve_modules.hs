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
import Control.Monad(forM,mapM,(>=>))

data Module' = Module' {moduleName' :: ModuleName, insideModule' :: InsideModule'}
data InsideModule' = Func (Either SemanticError (Input -> Output)) | ModuleChain' [ModuleName]

newtype InsideModule3 = Funct{unFunct :: (Either SemanticError (Input -> Output))}
type Resmap = M.Map ModuleName InsideModule3

data ModuleError = ME{unME::SemanticError}

modulesToFunc :: Set Module -> Either SemanticError (Input -> Output)
modulesToFunc = modules'ToFunc . map moduleToModule'

-- return func from "_Main" module
modules'ToFunc :: Set Module' -> Either SemanticError (Input -> Output)
modules'ToFunc ms = do 
 resmap <- liftLeft unME $ modules'ToResmap ms
 case ModuleName (Id "_Main") `M.lookup` resmap of
  Just (Funct func) -> func
  Nothing -> error "CANNOT HAPPEN" -- _Main always exists!

modules'ToResmap :: Set Module' -> Either ModuleError Resmap
modules'ToResmap ms = msToR ms M.empty


msToR :: Set Module' -> Resmap -> Either ModuleError Resmap
msToR [] resmap = return resmap
msToR (Module'{moduleName' = name, insideModule' = Func func}:ms) resmap = do 
 newMap <- insertIfNew name (Funct func) resmap
 msToR ms newMap
msToR (m@Module'{moduleName' = name, insideModule' = ModuleChain' chain}:ms) resmap =
 case forM chain $ \n -> M.lookup n resmap of
  Nothing -> msToR (ms++[m]) resmap -- FIXME: infinite loop in case of cyclic dependency
  Just insides -> do
   let func = combineFuncs insides
   msToR ms (M.insert name (Funct func) resmap)

insertIfNew :: (ToSource a, Ord a) => a -> b -> M.Map a b -> Either ModuleError (M.Map a b)
insertIfNew a b m = case M.lookup a m of
 Nothing -> return $ M.insert a b m
 Just _ -> Left $ ME E{errNum = 523, errStr = "Duplicate definition of module {"++toSource a++"}"}

combineFuncs :: [InsideModule3] -> Either SemanticError (Input -> Output)
combineFuncs arr = do
 funcs <- mapM unFunct arr -- FIXME: Errors except the first one are discarded
 return $ foldr1 (>=>) funcs
 
liftLeft :: (a -> c) -> (Either a b -> Either c b)
liftLeft f (Left a) = Left $ f a
liftLeft _ (Right x) = Right x


moduleToModule' :: Module -> Module'
moduleToModule' Module {moduleName = name, insideModule = Sents sents}
 = Module'{moduleName' = name, insideModule' = Func (liftLeft f $ sentsToFunc sents)} where
  f :: SemanticError -> SemanticError
  f e = e{errStr = "Inside module "++ toSource name ++ ":\n"++ errStr e}  
moduleToModule' Module {moduleName = name, insideModule = ModuleChain chain}
 = Module'{moduleName' = name, insideModule' = ModuleChain' chain}

