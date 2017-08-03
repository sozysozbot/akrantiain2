{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

module Akrantiain.Lexer
(modules
) where
import Prelude hiding (undefined)
import Text.Parsec hiding(spaces)

import Control.Applicative ((<$>),(<*))
import Data.Char (isSpace,chr)
import Data.Maybe (catMaybes)
import Control.Monad(void,replicateM)
import Akrantiain.Structure
import Akrantiain.Modules
import Numeric(readHex)
import Akrantiain.Lexer2

type Parser = Parsec [Token] ()

---- parsing modules -----

modules :: Parser (Set Module)
modules = do
 mods <- many (try(comment >> return Nothing) <|> fmap Just parseModule)
 insideMain <- parseInside
 mods2 <- many (try(comment >> return Nothing) <|> fmap Just parseModule)
 eof
 return $ Module{moduleName = HiddenModule, insideModule = insideMain} : catMaybes mods ++ catMaybes mods2

{-
 foo
 A => B
 -}
oneModule :: Parser ModuleName
oneModule = try foo <|> fmap ModuleName identifier where
 foo = do{x <- identifier; spaces'; string "=>"; spaces'; y <- identifier; return Arrow{before=x, after=y}}

{-
 foo
 (A => B)
 A => B => C
 -}
modChainElem :: Parser [ModuleName]
modChainElem = try (p <* spaces') <|> try(char '(' *> spaces' *> p <* spaces' <* char ')' <* spaces') where
 p = fmap f ids
 f :: [Identifier] -> [ModuleName]
 f [] = error "CANNOT HAPPEN"
 f [x] = [ModuleName x]
 f [x,y] = [Arrow{before = x, after = y}]
 f (x:y:zs) = f[x,y] ++ f(y:zs)
 ids = identifier `sepBy1` try(spaces' >> string "=>" >> spaces')

{-
 foo >> bar
 A=>B >> baz >> B=>C
 foobar >> (A => B => C) >> barfoo
 -}
modChain :: Parser [ModuleName]
modChain = fmap concat $ modChainElem `sepBy1` try(string ">>" >> spaces')


execModules :: Parser InsideModule
execModules = do
 try $ string "%%"
 spaces'
 mods <- modChain
 sentTerminate
 return $ ModuleChain mods

parseModule :: Parser Module
parseModule = do
 modname <- try $ do
  char '%' >> spaces'
  oneModule
 spaces' >> char '{' >> spaces'
 inside <- parseInside
 spaces' >> char '}' >> spaces'
 return Module{moduleName = modname, insideModule = inside}

parseInside :: Parser InsideModule
parseInside = try execModules' <|> fmap Sents sentences where
 execModules' = skipMany comment *> execModules <* skipMany comment


---- parsing the rest -----

sentences :: Parser (Set Sentence)
sentences = do
 sents <- many (try(comment >> return Nothing) <|> try(fmap Just sentence))
 return $ catMaybes sents

comment :: Parser ()
comment = void space <|> void(try $ spaces' >> void(oneOf ";\n")) <|> (char '#' >> skipMany (noneOf "\n") >> (eof <|> void(char '\n')))



dollar :: Parser Phoneme
dollar = char '$' >> return Dollar


slashString :: Parser Phoneme
slashString = do
  char '/'
  str <- many(noneOf "\\/\n" <|> escapeSequence)
  char '/'
  return $ Slash str

identifier :: Parser Identifier
identifier = fmap Id $ (:) <$> letter <*> many (alphaNum <|> char '_')


select :: Parser Select
select = (char '^' >> return Boundary2) <|> fmap Iden identifier <|> try single <|> try mult  where
 single = (Pipe . Ch . (:[])) <$> quotedString
 mult = do
  char '('
  spaces'
  strings <- stringsSepByPipe
  spaces'
  char ')'
  return $ Pipe strings

stringsSepByPipe :: Parser (Choose Quote)
stringsSepByPipe = fmap Ch $ strs `sepBy1` try(char '|' >> spaces')
 where strs = concat' <$> many1(quotedString <* spaces')

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
  ch_quote <- stringsSepByPipe
  sentTerminate
  return $ Right'$Define ident ch_quote

spaces' :: Parser ()
spaces' = skipMany $ satisfy (\a -> isSpace a && a /= '\n')

sentTerminate :: Parser ()
sentTerminate = eof <|> comment

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
  let phoneme = dollar <|> slashString
  phonemes <- many1(try$phoneme <* spaces')
  sentTerminate
  return $ Left' Conversion{mid=selects, phons=phonemes, lneg=l, rneg=r}
   where
    neg_select = try $ fmap Just $ char '!' >> spaces' >> select

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



quotedString :: Parser Quote
quotedString = do
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
 sentTerminate
 return $ Middle' ide




concat' :: [Quote] -> Quote
concat' arr = Quote(arr >>= \(Quote a) -> a)
