{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Akrantiain.Resolve_modules
(module4sToFunc'
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
import Control.Monad((>=>))
import Akrantiain.MtoM4


type RMap = M.Map ModuleName InsideModule4

-- return func from HiddenModule
module4sToFunc' :: Set Module4 -> Either ModuleError (Input -> Output)
module4sToFunc' m4s = do
  rmap <- toRMap (map toTuple m4s)
  resolve (rmap,[]) HiddenModule

toTuple :: Module4 -> (ModuleName, InsideModule4)
toTuple Module4{moduleName4 = a, insideModule4 = b} = (a,b)

toRMap :: [(ModuleName, InsideModule4)] -> Either ModuleError RMap
toRMap list
 | length list == M.size (M.fromList list) = return $ M.fromList list -- No duplicate
 | otherwise = Left $ ME {errorNo = 1523, errorMsg = "Duplicate definition of module(s) "++str}
    where
     str = "{" ++ intercalate "}, {" (map toSource dupList)++ "}"
     dupList = map head . filter((> 1) . length) . group . sort . map fst $ list

type S = (RMap, [ModuleName])

resolve :: S -> ModuleName -> Either ModuleError (Input -> Output)
resolve (rmap,arr) name
 | name `elem` arr = Left $ ME {errorNo = 1112, errorMsg = "Circular reference involving module {" ++ toSource name ++ "}"}
 | otherwise = case name `M.lookup` rmap of
  Nothing -> Left $ ME {errorNo = 1111, errorMsg = "Module {" ++ toSource name ++ "} does not exist"}
  Just (Func4 func) -> return func
  Just (ModuleChain4 mods) -> do
   funcs <- mapM (resolve (rmap, name:arr)) mods
   return $ foldr1 (>=>) funcs
