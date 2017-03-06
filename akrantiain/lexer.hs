{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

module Akrantiain.Lexer
(parseTest
,dollar_int
,quoted_string
,slash_string
,conversion
,define
,sentences
) where

import Text.Parsec hiding(spaces)

import Control.Applicative ((<$>),(<*))
import Data.Char (isSpace)
import Text.Parsec.String (Parser)
import Data.Maybe (catMaybes)
import Control.Monad(void)
import Prelude hiding (undefined)
import Akrantiain.Structure


backquoted_string :: Parser [Orthography]
backquoted_string = do
  char '`'
  str <- many(noneOf "`\n")
  char '`'
  return $ fmap (Pos . Res) [Boundary, (Quo . Quote) str, Boundary] 

spaces' :: Parser ()
spaces' = skipMany $ satisfy (\a -> isSpace a && a /= '\n')



sentences :: Parser (Set Sentence)
sentences = do
 sents <- many (try(comment >> return Nothing) <|> try(fmap Just sentence))
 eof
 return $ catMaybes sents

comment :: Parser ()
comment = void(try $ spaces' >> oneOf ";\n") <|> (char '#' >> skipMany (noneOf "\n") >> (eof <|> void(char '\n')))

-- consonant = "a" | "b" "d" | cons2 | co "c" co 
define :: Parser Sentence
define = do
  ident <- try $ do
   spaces'
   ident' <- identifier
   spaces'
   char '='
   return ident'
  spaces'
  let candidates = fmap C $ try $ many(try $ try candidate <* spaces')
  cands_arr <- try candidates `sepBy` try(char '|' >> spaces') 
  sent_terminate
  return $ Define ident cands_arr

sent_terminate :: Parser ()
sent_terminate = eof <|> comment
  

candidate :: Parser Candidate
candidate = try(fmap (Res . Quo) quoted_string) <|> try(fmap Ide identifier) <|> (char '^' >> return (Res Boundary))
  

conversion :: Parser Sentence
conversion = do 
  orthos <- try $ do
   spaces'
   let ortho = fmap (:[]) (fmap Pos candidate <|> try(fmap Neg $ char '!' >> spaces' >> candidate)) <|> backquoted_string
   orthos' <- many(try$ortho <* spaces')
   string "->"
   return $ concat orthos'
  spaces'
  let phoneme = dollar_int <|> slash_string
  phonemes <- many(try$phoneme <* spaces')
  sent_terminate
  return $ Conversion orthos phonemes


sentence :: Parser Sentence
sentence = conversion <|> define 




-- FIXME: Escape sequence not yet implemented
slash_string :: Parser Phoneme
slash_string = try $ do
  char '/'
  str <- many(noneOf "/\n")
  char '/'
  return $ Slash str

-- FIXME: Escape sequence not yet implemented
quoted_string :: Parser Quote
quoted_string = do
  char '"'
  str <- many(noneOf "\"\n")
  char '"'
  return $ Quote str

dollar_int :: Parser Phoneme
dollar_int = try $ do
  char '$'
  num <- many digit
  return $ Dollar(read num)

  
identifier :: Parser Identifier
identifier = fmap Id $ (:) <$> letter <*> many (alphaNum <|> char '_')

