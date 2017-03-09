{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

module Akrantiain.Errors
(SemanticError(..)
,RuntimeError(..)
) where
import Prelude hiding (undefined)
data SemanticError = E {errNum :: Int, errStr :: String} deriving(Eq, Ord)
instance Show SemanticError where
 show E{errNum = n, errStr = str} = "Semantic error (error code #" ++ show n ++ ")\n" ++ str 

data RuntimeError = RE {errNo :: Int, errMsg :: String} deriving(Eq, Ord)
instance Show RuntimeError where
 show RE{errNo = n, errMsg = str} = "Runtime error (error code #" ++ show n ++ ")\n" ++ str 
 