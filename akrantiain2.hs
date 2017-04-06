{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
import Prelude hiding (undefined)
import Akrantiain.Lexer
import Text.Parsec
import System.Environment
import System.IO
import Akrantiain.Resolve_modules
import Control.Monad(forM_, when, void)
import System.Process
import System.Info
import Akrantiain.MtoM4



main :: IO ()
main = do
 args <- getArgs
 case args of
  []    -> explain
  (fname:xs) -> do
   when (os == "mingw32" && (null xs || head xs /= "--file") ) $ callCommand "chcp 65001 > nul"
   handle <- openFile fname ReadMode
   hSetEncoding handle utf8
   input <- hGetContents handle
   runParser modules () fname input >>>= \mods ->
    mapM2 moduleToModule4 mods >>>= \mod4s ->
    module4sToFunc mod4s >>>= \func ->
    interact' func


(>>>=) :: (Show a) => Either a b -> ( b -> IO ()) -> IO ()
Left  a >>>= _  = do
 hPrint stderr a
 hPutStrLn stderr "\n\nPress Enter after reading this message."
 void getLine
Right b >>>= f  = f b


interact' :: (Show a) => (String -> Either a String) -> IO ()
interact' f = do
 hSetEncoding stdin utf8
 s <- getContents
 forM_ (lines s) $ \line -> case f line of
   Right str -> putStrLn str
   Left a -> hPrint stderr a >> putStrLn ""

explain :: IO ()
explain = putStrLn "mi'e .akranti'ain." -- FIXME: better explanation required
