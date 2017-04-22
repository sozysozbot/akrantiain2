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
,mapM2
) where
import Prelude hiding (undefined)
import Data.Either(lefts,rights)
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

-- similar to mapM but keeps track of all errors
mapM2 :: (d -> Either c b) -> [d] -> Either [c] [b]
mapM2 f ds = let{es = map f ds; (ls,rs) = (lefts es, rights es)} in
 case ls of
  [] -> Right rs
  _ -> Left ls

data SemanticWarning = SemanticWarning {warnNum   :: Int, warnStr    :: String} deriving(Eq, Ord)
data RuntimeWarning  = RuntimeWarning  {warnNo    :: Int, warnMsg    :: String} deriving(Eq, Ord)
data ModuleWarning   = ModuleWarning   {warningNo :: Int, warningMsg :: String} deriving(Eq, Ord)

type SemanticMsg a = WriterT [SemanticWarning] (Either SemanticError) a
type RuntimeMsg  a = WriterT [RuntimeWarning]  (Either RuntimeError ) a
type ModuleMsg   a = WriterT [ModuleWarning]   (Either ModuleError  ) a
