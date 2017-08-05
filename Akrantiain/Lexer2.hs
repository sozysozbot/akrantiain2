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

op :: String -> Parser ()
op str = do
  satisfy' (f . fst)
  return ()
   where
    f (Op a) = str == a
    f _ = False
op_ :: Char -> Parser ()
op_ c = op [c]

---- parsing modules -----

modules :: Parser (Set Module)
modules = do
 mods <- many (try(newLine >> return Nothing) <|> fmap Just parseModule)
 insideMain <- parseInside
 mods2 <- many (try(newLine >> return Nothing) <|> fmap Just parseModule)
 eof
 return $ Module{moduleName = HiddenModule, insideModule = insideMain} : catMaybes mods ++ catMaybes mods2

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
modChainElem = try p <|> try(op_ '(' *> p <* op_ ')') where
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
modChain = fmap concat $ modChainElem `sepBy1` try(op ">>" )


execModules :: Parser InsideModule
execModules = do
 try $ op "%%"
 mods <- modChain
 sentTerminate
 return $ ModuleChain mods

parseModule :: Parser Module
parseModule = do
 modname <- try $ do
  op_ '%' 
  oneModule
 op_ '{' 
 inside <- parseInside
 op_ '}'
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
dollar = op_ '$' >> return Dollar





select :: Parser Select
select = (op_ '^' >> return Boundary2) <|> fmap Iden identifier <|> try single <|> try mult  where
 single = (Pipe . Ch . (:[])) <$> quotedString
 mult = do
  op_ '('
  strings <- stringsSepByPipe
  op_ ')'
  return $ Pipe strings

stringsSepByPipe :: Parser (Choose Quote)
stringsSepByPipe = fmap Ch $ strs `sepBy1` try(op_ '|')
 where strs = concat' <$> many1 quotedString

-- consonant = "a" | "b" "d" | cons2 | co "c" co
define :: Parser Sentence
define = do
  ident <- try $ do
   ident' <- identifier
   op_ '='
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
    neg_select = try $ fmap Just $ op_ '!'  >> select

sentence :: Parser Sentence
sentence = conversion <|> define <|> atsignOption

atsignOption :: Parser Sentence
atsignOption = do
 op_ '@'
 ide <- identifier
 sentTerminate
 return $ Middle' ide

concat' :: [Quote] -> Quote
concat' arr = Quote(arr >>= \(Quote a) -> a)




quotedString :: Parser Quote
quotedString = do
  Q i <- satisfy' (f . fst)
  return (Quote i)
   where
    f (Q _) = True
    f _ = False

slashString :: Parser Phoneme
slashString = do
  S i <- satisfy' (f . fst)
  return (Slash i)
   where
    f (S _) = True
    f _ = False

identifier :: Parser Identifier
identifier = do
  I i <- satisfy' (f . fst)
  return i
   where
    f (I _) = True
    f _ = False


newLine :: Parser ()
newLine = do
  satisfy' (f . fst)
  return ()
   where
    f NewLine = True
    f _ = False

