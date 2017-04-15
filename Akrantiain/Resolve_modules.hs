{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Akrantiain.Resolve_modules
(module4sToFunc
,module4sToFunc'
,mapM2
,Module4(..)
,InsideModule4(..)
) where
-- import Prelude hiding (undefined)
import Data.List(intercalate, sort, group)
import Akrantiain.Modules
import Akrantiain.Structure
import Akrantiain.Sents_to_rules
import Akrantiain.Errors
import qualified Data.Map as M
import Control.Monad(forM,(>=>))
import Akrantiain.MtoM4

newtype InsideModule5 = Functi{unFuncti :: (Input -> Output)}
type Resmap5 = M.Map ModuleName InsideModule5

type RMap = M.Map ModuleName InsideModule4

-- return func from HiddenModule
module4sToFunc' :: Set Module4 -> Either ModuleError (Input -> Output)
module4sToFunc' m4s = do
  rmap <- toRMap (map toTuple m4s)
  foo rmap HiddenModule

toTuple :: Module4 -> (ModuleName, InsideModule4)
toTuple Module4{moduleName4 = a, insideModule4 = b} = (a,b)

toRMap :: [(ModuleName, InsideModule4)] -> Either ModuleError RMap
toRMap list
 | length list == M.size (M.fromList list) = return $ M.fromList list -- No duplicate
 | otherwise = Left $ ME {errorNo = 1523, errorMsg = "Duplicate definition of module(s) "++str}
    where
     str = "{" ++ intercalate "}, {" (map toSource dupList)++ "}"
     dupList = map head . filter((> 1) . length) . group . sort . map fst $ list

foo :: RMap -> ModuleName -> Either ModuleError (Input -> Output)
foo rmap name = case name `M.lookup` rmap of
 Nothing -> Left $ ME {errorNo = 1111, errorMsg = "Module {" ++ toSource name ++ "} does not exist"}
 Just (Func4 func) -> return func
 Just (ModuleChain4 mods) -> do
  funcs <- mapM (foo rmap) mods
  return $ foldr1 (>=>) funcs


-- return func from "_Main" module
module4sToFunc :: Set Module4 -> Either ModuleError (Input -> Output)
module4sToFunc m4s = do
 resmap <- module4sToResmap m4s
 case HiddenModule `M.lookup` resmap of
  Just (Functi func) -> return func
  Nothing -> error "CANNOT HAPPEN" -- _Main always exists!

module4sToResmap :: Set Module4 -> Either ModuleError Resmap5
module4sToResmap ms = m4sToR ms M.empty

m4sToR :: Set Module4 -> Resmap5 -> Either ModuleError Resmap5
m4sToR [] resmap = return resmap
m4sToR (Module4{moduleName4 = name, insideModule4 = Func4 func}:ms) resmap = do
 newMap <- insertIfNew name (Functi func) resmap
 m4sToR ms newMap
m4sToR mss@(m@Module4{moduleName4 = name, insideModule4 = ModuleChain4 chain}:ms) resmap = if all isResolveNeeded mss
 then let strs = map (toSource . moduleName4) mss in
  Left $ ME {errorNo = 1652, errorMsg = "Unresolvable module(s) {" ++ intercalate "}, {" strs ++ "}"}
 else case forM chain $ \n -> M.lookup n resmap of
  Nothing -> m4sToR (ms++[m]) resmap
  Just insides -> do
   let func = combineFuncs insides
   m4sToR ms (M.insert name (Functi func) resmap)

isResolveNeeded :: Module4 -> Bool
isResolveNeeded Module4{moduleName4 = HiddenModule} = False
isResolveNeeded Module4{insideModule4 = Func4 _} = False
isResolveNeeded Module4{insideModule4 = ModuleChain4 _} = True

insertIfNew :: (ToSource a, Ord a) => a -> b -> M.Map a b -> Either ModuleError (M.Map a b)
insertIfNew a b m = case M.lookup a m of
 Nothing -> return $ M.insert a b m
 Just _ -> Left $ ME {errorNo = 1523, errorMsg = "Duplicate definition of module {"++toSource a++"}"}

combineFuncs :: [InsideModule5] -> (Input -> Output)
combineFuncs arr =
 let funcs = map unFuncti arr in
  foldr1 (>=>) funcs
