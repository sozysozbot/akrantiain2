{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

module Akrantiain.Errors
(SemanticError(..)
,RuntimeError(..)
,ModuleError(..)
,SemanticWarning(..)
,RuntimeWarning(..)
,ModuleWarning(..)
,SemanticMsg
,RuntimeMsg
,ModuleMsg
,lift,tell -- from Control.Monad.Writer
) where
import Prelude hiding (undefined)
import Control.Monad.Writer

data SemanticError = E {errNum :: Int, errStr :: String} deriving(Eq, Ord)
instance Show SemanticError where
 show E{errNum = n, errStr = str} = "Semantic error (error code #" ++ show n ++ ")\n" ++ str

data RuntimeError = RE {errNo :: Int, errMsg :: String} deriving(Eq, Ord)
instance Show RuntimeError where
 show RE{errNo = n, errMsg = str} = "Runtime error (error code #" ++ show n ++ ")\n" ++ str

data ModuleError = ME{errorNo :: Int, errorMsg :: String} deriving(Eq, Ord)
instance Show ModuleError where
 show ME{errorNo = n, errorMsg = str} = "Module error (error code #" ++ show n ++ ")\n" ++ str

 
 

data SemanticWarning = SemanticWarning {warnNum   :: Int, warnStr    :: String} deriving(Eq, Ord)
instance Show SemanticWarning where
 show SemanticWarning{warnNum = n, warnStr = str} = "Semantic warning (warning code #" ++ show n ++ ")\n" ++ str

data RuntimeWarning  = RuntimeWarning  {warnNo    :: Int, warnMsg    :: String} deriving(Eq, Ord)
instance Show RuntimeWarning where
 show RuntimeWarning{warnNo = n, warnMsg = str} = "Runtime warning (warning code #" ++ show n ++ ")\n" ++ str

data ModuleWarning   = ModuleWarning   {warningNo :: Int, warningMsg :: String} deriving(Eq, Ord)
instance Show ModuleWarning where
 show ModuleWarning{warningNo = n, warningMsg = str} = "Module warning (warning code #" ++ show n ++ ")\n" ++ str

type SemanticMsg a = WriterT [SemanticWarning] (Either SemanticError) a
type RuntimeMsg  a = WriterT [RuntimeWarning]  (Either RuntimeError ) a
type ModuleMsg   a = WriterT [ModuleWarning]   (Either ModuleError  ) a
