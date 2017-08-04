{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

module Akrantiain.Tokenizer
(toTokens
) where
import Prelude hiding (undefined)
import Text.Parsec hiding(spaces)

import Control.Applicative ((<$>),(<*))
import Data.Char (isSpace,chr)
import Text.Parsec.String (Parser)
import Control.Monad(void,replicateM)
import Akrantiain.Structure
import Akrantiain.Modules
import Numeric(readHex)

data Tok = I Identifier | S String | Q String | Op String | NewLine deriving(Eq,Ord,Show)
type Token = (Tok, SourcePos)

toTokens :: Parser [Token]
toTokens = (many (optional (try spaces'__) >> try (po tok))) <* eof
 where
  tok = try operator__ <|> try identifier__ <|> try slashString__ <|> try quotedString__ <|> try newline__ 

po :: Parser a -> Parser (a,SourcePos)
po x = (,) <$> x <*> getPosition 

escapeSequence :: Parser Char
escapeSequence =
 try(string "\\\\" >> return '\\') <|>
 try(string "\\\"" >> return '"')  <|>
 try(string "\\/" >> return '/') <|> try uni where
  uni = do
   string "\\u"
   hexes <- replicateM 4 hexDigit
   let [(num,"")] = readHex hexes 
   return $ chr num

operator__ :: Parser Tok
operator__ = try $ foldl1 (<|>) $ map (fmap Op . try . string) ops
 where ops = [">>","%%", "%", "{", "}", "$", "=>", "(", "^", "(", ")", "|", "=", "->", "!", "@"]

quotedString__ :: Parser Tok
quotedString__ = do
  char '"'
  str <- many(noneOf "\\\"\n" <|> escapeSequence)
  char '"'
  return $ Q str 


newline__ :: Parser Tok
newline__ = c >> return NewLine
 where c = void(try $ spaces'__ >> void(oneOf ";\n")) <|> (char '#' >> skipMany (noneOf "\n") >> (eof <|> void(char '\n')))

spaces'__ :: Parser ()
spaces'__ = try $ skipMany $ satisfy (\a -> isSpace a && a /= '\n')

slashString__ :: Parser Tok
slashString__ = do
  char '/'
  str <- many(noneOf "\\/\n" <|> escapeSequence)
  char '/'
  return $ S str

identifier__ :: Parser Tok
identifier__ = fmap (I . Id) $ (:) <$> letter <*> many (alphaNum <|> char '_')

