{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

module Akrantiain.Lexer2
(toTokens
,Token
) where
import Prelude hiding (undefined)
import Text.Parsec hiding(spaces)

import Control.Applicative ((<$>),(<*))
import Data.Char (isSpace,chr)
import Text.Parsec.String (Parser)
import Data.Maybe (catMaybes)
import Control.Monad(void,replicateM)
import Akrantiain.Structure
import Akrantiain.Modules
import Numeric(readHex)

type Token = Char

toTokens :: Parser [Token]
toTokens = many anyChar