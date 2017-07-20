{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Akrantiain.Resolve_modules
(module4sToFunc'
,Module4(..)
,InsideModule4(..)
) where
import Prelude hiding (undefined)
import Data.List(sort, group)
import Akrantiain.Modules
import Akrantiain.Structure
import Akrantiain.Sents_to_func
import Akrantiain.Errors
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad((>=>), unless)
import Akrantiain.MtoM4
import Control.Monad.Writer


type RMap = M.Map ModuleName InsideModule4

-- return func from HiddenModule
module4sToFunc' :: Set Module4 -> ModuleMsg (Input -> Output)
module4sToFunc' m4s = do
  rmap <- lift $ toRMap (map toTuple m4s)
  resolve rmap HiddenModule

toTuple :: Module4 -> (ModuleName, InsideModule4)
toTuple Module4{moduleName4 = a, insideModule4 = b} = (a,b)

toRMap :: [(ModuleName, InsideModule4)] -> Either ModuleError RMap
toRMap list
 | length list == M.size (M.fromList list) = return $ M.fromList list -- No duplicate
 | otherwise = Left ME {errorNo = 1113, errorMsg = "Duplicate definition of module(s) " ++ toBraces dupList}
    where
     dupList = map head . filter((> 1) . length) . group . sort . map fst $ list

type S = (RMap, [ModuleName]) 
-- snd is the `call stack` used to detect circular reference



resolve :: RMap -> ModuleName -> ModuleMsg (Input -> Output)
resolve rmap name = do 
 (func, mods') <- lift $ runWriterT (resolve' (rmap,[]) name)
 let allmods = S.fromList . map fst . M.toList $ rmap
 let unused = allmods S.\\ S.fromList mods'
 unless (S.null unused) $ 
  tell [ModuleWarning{warningNo = 2000, warningMsg = "Unused module(s) " ++ toBraces (S.toList unused)}]
 return func

resolve' :: S -> ModuleName -> WriterT [ModuleName] (Either ModuleError) (Input -> Output)
resolve' (rmap,arr) name
 | name `elem` arr = lift $ Left ME {errorNo = 1112, errorMsg = "Circular reference involving module {" ++ toSource name ++ "}"}
 | otherwise = tell [name] >> case name `M.lookup` rmap of
  Nothing -> lift $ Left ME {errorNo = 1111, errorMsg = "Module {" ++ toSource name ++ "} does not exist"}
  Just (Func4 func) -> return func
  Just (ModuleChain4 mods) -> do
   funcs <- mapM (resolve' (rmap, name:arr)) mods
   return $ foldr1 (>=>) funcs
