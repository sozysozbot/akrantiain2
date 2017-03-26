{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

module Akrantiain.Lexer
(parseTest
,quoted_string
,slash_string
,conversion
,define
,sentences
) where
import Prelude hiding (undefined)
import Text.Parsec hiding(spaces)

import Control.Applicative ((<$>),(<*))
import Data.Char (isSpace)
import Text.Parsec.String (Parser)
import Data.Maybe (catMaybes)
import Control.Monad(void)
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


slash_string :: Parser Phoneme
slash_string = do
  char '/'
  str <- many(noneOf "\\/\n" <|> escapeSequence)
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
  return $ Pipe strings

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
  return $ Right'$Define ident ch_quote

spaces' :: Parser ()
spaces' = skipMany $ satisfy (\a -> isSpace a && a /= '\n')

sent_terminate :: Parser ()
sent_terminate = eof <|> comment

conversion :: Parser Sentence
conversion = do 
  (selects,l,r) <- try $ do
   spaces'
   left <- option Nothing neg_select
   spaces'
   selects' <- many1(try$select <* spaces')
   spaces'
   right <- option Nothing neg_select
   spaces'
   string "->"
   return (selects',left,right)
  spaces'
  let phoneme = dollar <|> slash_string
  phonemes <- many1(try$phoneme <* spaces')
  sent_terminate
  return $ Left' Conversion{mid=selects, phons=phonemes, lneg=l, rneg=r}
   where
    neg_select = try $ fmap Just $ char '!' >> spaces' >> select

escapeSequence :: Parser Char
escapeSequence = 
 try(string "\\\\" >> return '\\') <|>  
 try(string "\\\"" >> return '"')  <|>  
 try(string "\\/" >> return '/') <|>  
 try(string "\\'" >> return '\'') 

quoted_string :: Parser Quote
quoted_string = do
  char '"'
  str <- many(noneOf "\\\"\n" <|> escapeSequence)
  char '"'
  return $ Quote str

sentence :: Parser Sentence
sentence = conversion <|> define <|> atsignOption

atsignOption :: Parser Sentence
atsignOption = do
 char '@' 
 spaces' 
 ide <- identifier 
 spaces'
 sent_terminate
 return $ Middle' ide




concat' :: [Quote] -> Quote
concat' arr = Quote(arr >>= \(Quote a) -> a)








  


