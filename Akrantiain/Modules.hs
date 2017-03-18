{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

module Akrantiain.Modules
(Module(..)
,ModuleName(..)
,InsideModule(..)
) where
import Prelude hiding (undefined)
import Akrantiain.Structure
data Module = Module {moduleName :: ModuleName, insideModule :: InsideModule}
data ModuleName = Arrow {before :: Identifier, after :: Identifier} | ModuleName Identifier deriving(Show, Eq, Ord)
data InsideModule = Sents [Sentence] | ModuleChain [ModuleName]

instance ToSource ModuleName where
 toSource (ModuleName i) = toSource i
 toSource (Arrow bef aft) = "(" ++ toSource bef ++ " => " ++ toSource aft ++ ")"
