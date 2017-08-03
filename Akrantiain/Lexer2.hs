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
import Control.Monad(void,replicateM)
import Akrantiain.Structure
import Numeric(readHex)

type Token = Either Tok Char

data Tok = I Identifier | S String | Q String | Op String | NewLine deriving(Show)

toTokens :: Parser [Token]
toTokens = fmap (map Left) (optional spaces'__ >> many tok) <* eof
 where
  tok = newline__ <|> identifier__ <|> slashString__ <|> quotedString__ <|> operator__

char :: Char -> Parsec [Token] () ()
char a = string [a]

string :: String -> Parsec [Token] () ()
string = undefined


comment :: Parsec [Token] () ()
comment = return ()


spaces' :: Parsec [Token] () ()
spaces' = return ()

identifier :: Parsec [Token] () Identifier
identifier = undefined

slashString :: Parsec [Token] () Phoneme
slashString = undefined

quotedString :: Parsec [Token] () Quote
quotedString = undefined

operator__ :: Parser Tok
operator__ = undefined

comment__ :: Parser ()
comment__ = void space <|> void newline__

newline__ :: Parser Tok
newline__ = c >> return NewLine
 where c = void(try $ spaces'__ >> void(oneOf ";\n")) <|> (T.char '#' >> skipMany (noneOf "\n") >> (eof <|> void(T.char '\n')))

spaces'__ :: Parser ()
spaces'__ = skipMany $ satisfy (\a -> isSpace a && a /= '\n')

identifier__ :: Parser Tok
identifier__ = fmap (I . Id) $ (:) <$> letter <*> many (alphaNum <|> T.char '_')

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

slashString__ :: Parser Tok
slashString__ = do
  T.char '/'
  str <- many(noneOf "\\/\n" <|> escapeSequence__)
  T.char '/'
  return $ S str

quotedString__ :: Parser Tok
quotedString__ = do
  T.char '"'
  str <- many(noneOf "\\\"\n" <|> escapeSequence__)
  T.char '"'
  return $ Q str 
