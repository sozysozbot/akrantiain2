{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Akrantiain.Resolve_modules
(module4sToFunc'
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
import qualified Data.Set as S
import Control.Monad((>=>), forM)
import Akrantiain.MtoM4
import Control.Monad.Reader


type RMap = M.Map ModuleName InsideModule4

-- return func from HiddenModule
module4sToFunc' :: Set Module4 -> ModuleMsg (Input -> Output)
module4sToFunc' m4s = do
  rmap <- lift $ toRMap (map toTuple m4s)
  resolve' (rmap,[]) HiddenModule

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
-- snd is the `call stack` used to detect circular reference

resolve' :: S -> ModuleName -> ModuleMsg (Input -> Output)
resolve' s name = lift $ resolve s name `runReaderT` S.empty

resolve :: S -> ModuleName -> ReaderT (S.Set ModuleName) (Either ModuleError) (Input -> Output)
resolve (rmap, stack) name 
 | name `elem` stack = ReaderT $ \set -> Left $ ME {errorNo = 1112, errorMsg = "Circular reference involving module {" ++ toSource name ++ "}"}
 | otherwise = case name `M.lookup` rmap of
  Nothing -> ReaderT $ \set -> Left $ ME {errorNo = 1111, errorMsg = "Module {" ++ toSource name ++ "} does not exist"}
  Just (Func4 func) -> ReaderT $ \set -> return func
  Just (ModuleChain4 mods) -> ReaderT $ \set -> do
   funcs <- forM mods $ \modu -> resolve (rmap, name:stack) modu `runReaderT` set
   return $ foldr1 (>=>) funcs
