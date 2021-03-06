{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

module Akrantiain.Tokenizer
(toTokens
,Tok(..),Token
) where
import Prelude hiding (undefined)
import Text.Parsec hiding(spaces)

import Control.Applicative ((<$>))
import Data.Char (isSpace,chr)
import Text.Parsec.String (Parser)
import Control.Monad(void,replicateM)
import Akrantiain.Structure
import Numeric(readHex)

data Tok = I Identifier | S String | Q String | Op String | NewLine deriving(Eq,Ord,Show)
type Token = (Tok, SourcePos)

instance ToSource Tok where
  toSource (I ide) = toSource ide
  toSource (S str) = toSource $ Slash str
  toSource (Q str) = toSource $ Quote str
  toSource (Op str) = str
  toSource NewLine = show "\n"

toTokens :: Parser [Token]
toTokens = do
  toks <- many $ try $ try(optional spaces'__) >> try ( (,) <$> tok <*> getPosition )
  return $ toks >>= foo
 where
  tok = try operator__ <|> try identifier__ <|> try slashString__ <|> try quotedString__ <|> try newline__ 
  foo :: Token -> [Token]
  foo a@(Op "}", pos) = [(NewLine, pos), a]
  foo x = [x]

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
operator__ = (<?> "operator") $ foldl1 (<|>) $ map (fmap Op . try . string) ops
 where ops = [">>","%%", "%", "{", "}", "$", "=>", "(", "^", "(", ")", "|", "=", "->", "!", "@"]

quotedString__ :: Parser Tok
quotedString__ = (<?> "quoted string") $ do
  char '"'
  str <- many(noneOf "\\\"\n" <|> escapeSequence)
  char '"'
  return $ Q str 

newline__ :: Parser Tok
newline__ = (<?> "newline") $ c >> return NewLine
 where c = void(oneOf ";\n") <|> (char '#' >> skipMany (noneOf "\n") >> (eof <|> void(char '\n')))

spaces'__ :: Parser ()
spaces'__ = (<?> "spaces") $ skipMany $ satisfy (\a -> isSpace a && a /= '\n')

slashString__ :: Parser Tok
slashString__ = (<?> "slash string") $ do
  char '/'
  str <- many(noneOf "\\/\n" <|> escapeSequence)
  char '/'
  return $ S str

identifier__ :: Parser Tok
identifier__ = (<?> "identifier") $ fmap (I . Id) $ (:) <$> letter <*> many (alphaNum <|> char '_')

