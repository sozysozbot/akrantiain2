{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Akrantiain.Resolve_modules
(modulesToFunc
) where
-- import Prelude hiding (undefined)
import Akrantiain.Modules
import Akrantiain.Structure
import Akrantiain.Sents_to_rules
import Akrantiain.Errors
{-
sentsToFunc :: Set Sentence -> Either SemanticError (Input -> Output)
-}

-- return func from "_Main" module
modulesToFunc :: Set Module -> Either SemanticError (Input -> Output)
modulesToFunc = undefined


