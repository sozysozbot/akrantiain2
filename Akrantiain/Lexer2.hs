{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

module Akrantiain.Lexer2
(toTokens
,Token
,char,string
,escapeSequence
,quotedString
,identifier
,comment
,spaces'
) where
import Prelude hiding (undefined)
import Text.Parsec hiding(spaces,char,string)
import qualified Text.Parsec as T

import Control.Applicative ((<$>),(<*))
import Data.Char (isSpace,chr)
import Text.Parsec.String (Parser)
import Data.Maybe (catMaybes)
import Control.Monad(void,replicateM)
import Akrantiain.Structure
import Numeric(readHex)

type Token = Char

toTokens :: Parser [Token]
toTokens = many anyChar <* eof

char :: Char -> Parsec [Token] () Char
char = T.char

string :: String -> Parsec [Token] () String
string = T.string

comment :: Parser ()
comment = void space <|> void(try $ spaces' >> void(oneOf ";\n")) <|> (char '#' >> skipMany (noneOf "\n") >> (eof <|> void(char '\n')))


spaces' :: Parser ()
spaces' = skipMany $ satisfy (\a -> isSpace a && a /= '\n')

identifier :: Parser Identifier
identifier = fmap Id $ (:) <$> letter <*> many (alphaNum <|> T.char '_')

escapeSequence :: Parser Char
escapeSequence =
 try(T.string "\\\\" >> return '\\') <|>
 try(T.string "\\\"" >> return '"')  <|>
 try(T.string "\\/" >> return '/') <|> try uni where
  uni = do
   T.string "\\u"
   hexes <- replicateM 4 hexDigit
   let [(num,"")] = readHex hexes 
   return $ chr num



quotedString :: Parser Quote
quotedString = do
  T.char '"'
  str <- many(noneOf "\\\"\n" <|> escapeSequence)
  T.char '"'
  return $ Quote str 
