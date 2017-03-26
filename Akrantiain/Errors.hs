{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

module Akrantiain.Errors
(SemanticError(..)
,RuntimeError(..)
,ModuleError(..)
,mapM2
) where
import Prelude hiding (undefined)
import Data.Either(lefts,rights)

data SemanticError = E {errNum :: Int, errStr :: String} deriving(Eq, Ord)
instance Show SemanticError where
 show E{errNum = n, errStr = str} = "Semantic error (error code #" ++ show n ++ ")\n" ++ str 

data RuntimeError = RE {errNo :: Int, errMsg :: String} deriving(Eq, Ord)
instance Show RuntimeError where
 show RE{errNo = n, errMsg = str} = "Runtime error (error code #" ++ show n ++ ")\n" ++ str 
 
data ModuleError = ME{errorNo :: Int, errorMsg :: String} deriving(Eq, Ord)
instance Show ModuleError where
 show ME{errorNo = n, errorMsg = str} = "Module error (error code #" ++ show n ++ ")\n" ++ str  
 
-- similar to mapM but keeps track of all errors
mapM2 :: (d -> Either c b) -> [d] -> Either [c] [b]
mapM2 f ds = let{es = map f ds; (ls,rs) = (lefts es, rights es)} in 
 case ls of
  [] -> Right rs
  _ -> Left ls
