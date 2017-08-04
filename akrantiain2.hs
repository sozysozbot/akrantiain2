{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
import Prelude hiding (undefined)
import Akrantiain.Lexer
import qualified Akrantiain.Lexer2 as L2
import Text.Parsec
import System.Environment
import System.IO
import Akrantiain.Resolve_modules
import Control.Monad(forM_, when, void)
import System.Process
import System.Info
import Akrantiain.MtoM4
import Control.Monad.Writer
import Akrantiain.Errors2
import Data.Aeson
import Akrantiain.Modules(Module2s(..))
import qualified Data.ByteString.Lazy.Char8 as B
import Akrantiain.Tokenizer


main :: IO ()
main = getArgs >>= main' False

-- Bool :: trigger toJSON
main' :: Bool -> [String] -> IO ()
main' _ []    = explain
main' _ xs
 | "--toJSON" `elem` xs = main' True $ filter (/="--toJSON") xs
main' False (fname:xs) = do
   when (os == "mingw32" && (null xs || head xs /= "--file") ) $ callCommand "chcp 65001 > nul"
   input <- readFrom fname
   runParser toTokens () fname input >>>= \toks ->
    runParser L2.modules () fname toks >>>= \mods -> -- handles ParseError
    mapM3 moduleToModule4 mods >>== \mod4s -> -- handles SemanticError and SemanticWarning
    module4sToFunc' mod4s >>== \func -> -- handles ModuleError and ModuleWarning
    interact' func
main' True (fname:_) = do
   input <- readFrom fname
   runParser toTokens () fname input >>>= \toks ->
    runParser L2.modules () fname toks >>>= \mods -> 
    mapM3 moduleToModule2 mods >>== \mod2s -> B.putStrLn . encode $ Module2s mod2s

readFrom :: FilePath -> IO String
readFrom fname = do
   handle <- openFile fname ReadMode
   hSetEncoding handle utf8
   hGetContents handle

(>>>=) :: (Show a) => Either a b -> ( b -> IO ()) -> IO ()
Left  a >>>= _  = do
 hPrint stderr a
 hPutStrLn stderr "\n\nPress Enter after reading this message."
 void getLine
Right b >>>= f  = f b

(>>==) :: (Show w, Show a) => WriterT [w] (Either a) b -> (b -> IO ()) -> IO ()
wr >>== func = runWriterT wr >>>= func' where
 func' (b, w) = do
  mapM_ (hPrint stderr) w
  func b



interact' :: (Show a) => (String -> Either a String) -> IO ()
interact' f = do
 hSetEncoding stdin utf8
 s <- getContents
 forM_ (lines s) $ \line -> case f line of
   Right str -> putStrLn str
   Left a -> hPrint stderr a >> putStrLn ""

explain :: IO ()
explain = putStrLn "akrantiain (ver 0.6.13)\na domain-specific language designed to describe conlangs' orthographies" -- FIXME: better explanation required
