{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

module Akrantiain.Lexer
(parseTest
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

sentences :: Parser (Set Sentence)
sentences = do
 sents <- many (try(comment >> return Nothing) <|> try(fmap Just sentence))
 eof
 return $ catMaybes sents

comment :: Parser ()
comment = void space <|> void(try $ spaces' >> void(oneOf ";\n")) <|> (char '#' >> skipMany (noneOf "\n") >> (eof <|> void(char '\n')))



dollar :: Parser Phoneme
dollar = char '$' >> return Dollar


-- FIXME: Escape sequence not yet implemented
slash_string :: Parser Phoneme
slash_string = do
  char '/'
  str <- many(noneOf "/\n")
  char '/'
  return $ Slash str
  
identifier :: Parser Identifier
identifier = fmap Id $ (:) <$> letter <*> many (alphaNum <|> char '_')


select :: Parser Select
select = (char '^' >> return Boundary2) <|> fmap Iden identifier <|> try single <|> try mult  where
 single = (Pipe . Ch . (:[])) <$> quoted_string 
 mult = do
  char '('
  spaces'
  strings <- strings_sepBy_pipe
  spaces'
  char ')'
  return $ Pipe $ strings

strings_sepBy_pipe :: Parser (Choose Quote)
strings_sepBy_pipe = fmap Ch $ strs `sepBy1` try(char '|' >> spaces')
 where strs = concat' <$> many1(quoted_string <* spaces')

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
  ch_quote <- strings_sepBy_pipe
  sent_terminate
  return $ Define ident ch_quote







backquoted_string :: Parser [PNCandidate]
backquoted_string = do
  char '`'
  str <- many(noneOf "`\n")
  char '`'
  return $ fmap (Pos . Res) [Boundary, (Quo . Quote) str, Boundary] 

spaces' :: Parser ()
spaces' = skipMany $ satisfy (\a -> isSpace a && a /= '\n')









sent_terminate :: Parser ()
sent_terminate = eof <|> comment

candidates_p :: Parser Term
candidates_p = fmap C $ try $ many(try $ try pncandidate <* spaces')

options_p :: Parser Options
options_p = fmap F $ try candidates_p `sepBy` try(char '|' >> spaces') 

candidate :: Parser Candidate
candidate = try(fmap (Res . Quo) quoted_string) <|> try(fmap Ide identifier) <|> (char '^' >> return (Res Boundary))

pncandidate :: Parser PNCandidate
pncandidate = fmap Pos candidate <|> fmap Neg (try(char '!' >> candidate)) 

conversion :: Parser Sentence
conversion = do 
  orthos <- try $ do
   spaces'
   orthos' <- many(try$ortho <* spaces')
   string "->"
   return orthos'
  spaces'
  let phoneme = dollar <|> slash_string
  phonemes <- many(try$phoneme <* spaces')
  sent_terminate
  return $ Conversion orthos phonemes

ortho :: Parser Options
ortho = paren <|> fmap (F . (:[]) . C) (
 fmap(:[]) (fmap Pos candidate <|> try(fmap Neg $ char '!' >> spaces' >> candidate)) <|> backquoted_string)

paren :: Parser Options
paren = do 
 try $ do
  char '('
  spaces'
 a <- options_p
 spaces'
 char ')'
 return a
  
sentence :: Parser Sentence
sentence = conversion <|> define 




-- FIXME: Escape sequence not yet implemented
quoted_string :: Parser Quote
quoted_string = do
  char '"'
  str <- many(noneOf "\"\n")
  char '"'
  return $ Quote str

concat' :: [Quote] -> Quote
concat' arr = Quote(arr >>= \(Quote a) -> a)
  


