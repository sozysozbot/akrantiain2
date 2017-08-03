{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

module Akrantiain.Lexer2
(toTokens
,Token
,char,string
,quotedString
,identifier
,comment
,spaces'
,slashString
) where
--import Prelude hiding (undefined)
import Text.Parsec hiding(spaces,char,string)
import qualified Text.Parsec as T

import Control.Applicative ((<$>),(<*))
import Data.Char (isSpace,chr)
import Text.Parsec.String (Parser)
import Data.Maybe (catMaybes)
import Control.Monad(void,replicateM)
import Akrantiain.Structure
import Numeric(readHex)

type Token = Either () Char

toTokens :: Parser [Token]
toTokens = fmap (map Right) (many anyChar) <* eof

char :: Char -> Parsec [Token] () ()
char a = string [a]

string :: String -> Parsec [Token] () ()
string = undefined -- void . T.string

comment :: Parsec [Token] () ()
comment = undefined


spaces' :: Parsec [Token] () ()
spaces' = undefined

identifier :: Parsec [Token] () Identifier
identifier = undefined

slashString :: Parsec [Token] () Phoneme
slashString = undefined

quotedString :: Parsec [Token] () Quote
quotedString = undefined

comment__ :: Parser ()
comment__ = void space <|> void(try $ spaces'__ >> void(oneOf ";\n")) <|> (T.char '#' >> skipMany (noneOf "\n") >> (eof <|> void(T.char '\n')))

spaces'__ :: Parser ()
spaces'__ = skipMany $ satisfy (\a -> isSpace a && a /= '\n')

identifier__ :: Parser Identifier
identifier__ = fmap Id $ (:) <$> letter <*> many (alphaNum <|> T.char '_')

escapeSequence__ :: Parser Char
escapeSequence__ =
 try(T.string "\\\\" >> return '\\') <|>
 try(T.string "\\\"" >> return '"')  <|>
 try(T.string "\\/" >> return '/') <|> try uni where
  uni = do
   T.string "\\u"
   hexes <- replicateM 4 hexDigit
   let [(num,"")] = readHex hexes 
   return $ chr num

slashString__ :: Parser Phoneme
slashString__ = do
  T.char '/'
  str <- many(noneOf "\\/\n" <|> escapeSequence__)
  T.char '/'
  return $ Slash str

quotedString__ :: Parser Quote
quotedString__ = do
  T.char '"'
  str <- many(noneOf "\\\"\n" <|> escapeSequence__)
  T.char '"'
  return $ Quote str 
