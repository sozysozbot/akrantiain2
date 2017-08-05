{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

module Akrantiain.Lexer2
(modules
) where
--import Prelude hiding (undefined)
import Text.Parsec hiding(spaces)

import Control.Applicative ((<$>),(<*))
import Data.Maybe (catMaybes)
import Akrantiain.Structure
import Akrantiain.Modules
import Akrantiain.Tokenizer

type Parser = Parsec [Token] ()
satisfy' :: (Token -> Bool) -> Parsec [Token] u Tok
satisfy' f = fst <$> token showTok posFromTok testTok
   where
     showTok (t,_)     = toSource t
     posFromTok  = snd
     testTok t     = if f t then Just t else Nothing

sat :: (Tok -> Bool) -> Parsec [Token] u Tok
sat f = satisfy' (f . fst)

op :: String -> Parser ()
op str = void $ sat f
   where
    f (Op a) = str == a
    f _ = False

---- parsing modules -----

modules :: Parser (Set Module)
modules = do
 mods <- many (try(optional newLine) >> parseModule)
 insideMain <- parseInside
 mods2 <- many (try(optional newLine) >> parseModule)
 eof
 return $ Module{moduleName = HiddenModule, insideModule = insideMain} : mods ++ mods2

{-
 foo
 A => B
 -}
oneModule :: Parser ModuleName
oneModule = try foo <|> fmap ModuleName identifier where
 foo = do{x <- identifier; op "=>"; y <- identifier; return Arrow{before=x, after=y}}

{-
 foo
 (A => B)
 A => B => C
 -}
modChainElem :: Parser [ModuleName]
modChainElem = try p <|> try(op "(" *> p <* op ")") where
 p = fmap f ids
 f :: [Identifier] -> [ModuleName]
 f [] = error "CANNOT HAPPEN"
 f [x] = [ModuleName x]
 f [x,y] = [Arrow{before = x, after = y}]
 f (x:y:zs) = f[x,y] ++ f(y:zs)
 ids = identifier `sepBy1` try(op "=>")

{-
 foo >> bar
 A=>B >> baz >> B=>C
 foobar >> (A => B => C) >> barfoo
 -}
modChain :: Parser [ModuleName]
modChain = fmap concat $ modChainElem `sepBy1` try(op ">>")


execModules :: Parser InsideModule
execModules = do
 try $ op "%%"
 mods <- modChain
 sentTerminate
 return $ ModuleChain mods

parseModule :: Parser Module
parseModule = do
 modname <- try $ do
  op "%" 
  oneModule
 op "{" 
 inside <- parseInside
 op "}"
 return Module{moduleName = modname, insideModule = inside}

parseInside :: Parser InsideModule
parseInside = try execModules' <|> fmap Sents sentences where
 execModules' = skipMany newLine *> execModules <* skipMany newLine


---- parsing the rest -----

sentences :: Parser (Set Sentence)
sentences = do
 sents <- many (try(newLine >> return Nothing) <|> try(fmap Just sentence))
 return $ catMaybes sents



dollar :: Parser Phoneme
dollar = op "$" >> return Dollar





select :: Parser Select
select = (op "^" >> return Boundary2) <|> fmap Iden identifier <|> try single <|> try mult  where
 single = (Pipe . Ch . (:[])) <$> quotedString
 mult = do
  op "("
  strings <- stringsSepByPipe
  op ")"
  return $ Pipe strings

stringsSepByPipe :: Parser (Choose Quote)
stringsSepByPipe = fmap Ch $ strs `sepBy1` try(op "|")
 where strs = concat' <$> many1 quotedString

-- consonant = "a" | "b" "d" | cons2 | co "c" co
define :: Parser Sentence
define = do
  ident <- try $ do
   ident' <- identifier
   op "="
   return ident'
  ch_quote <- stringsSepByPipe
  sentTerminate
  return $ Right'$Define ident ch_quote


sentTerminate :: Parser ()
sentTerminate = eof <|> newLine

conversion :: Parser Sentence
conversion = do
  (selects,l,r) <- try $ do
   left <- option Nothing neg_select
   selects' <- many1(try$select)
   right <- option Nothing neg_select
   op "->"
   return (selects',left,right)
  let phoneme = dollar <|> slashString
  phonemes <- many1(try$phoneme)
  sentTerminate
  return $ Left' Conversion{mid=selects, phons=phonemes, lneg=l, rneg=r}
   where
    neg_select = try $ fmap Just $ op "!"  >> select

sentence :: Parser Sentence
sentence = conversion <|> define <|> atsignOption

atsignOption :: Parser Sentence
atsignOption = do
 op "@"
 ide <- identifier
 sentTerminate
 return $ Middle' ide

concat' :: [Quote] -> Quote
concat' arr = Quote(arr >>= \(Quote a) -> a)



-- simple parser

quotedString :: Parser Quote
quotedString = do
  Q i <- sat f
  return (Quote i)
   where
    f (Q _) = True
    f _ = False

slashString :: Parser Phoneme
slashString = do
  S i <- sat f
  return (Slash i)
   where
    f (S _) = True
    f _ = False

identifier :: Parser Identifier
identifier = do
  I i <- sat f
  return i
   where
    f (I _) = True
    f _ = False


newLine :: Parser ()
newLine = void $ sat f
   where
    f NewLine = True
    f _ = False

