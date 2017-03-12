{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
import Prelude hiding (undefined)
import Akrantiain.Lexer
import Text.Parsec
import System.Environment
import System.IO
import Akrantiain.Sents_to_rules
import Control.Monad(forM_, when, void)
import System.Process
import System.Info



main :: IO ()
main = do
 args <- getArgs
 case args of
  []    -> putStrLn "mi'e .akranti'ain."
  (fname:xs) -> do
   when (os == "mingw32" && (null xs || head xs /= "--file") ) $ callCommand "chcp 65001 > nul"
   handle <- openFile fname ReadMode 
   hSetEncoding handle utf8
   input <- hGetContents handle
   runParser sentences () fname input >>>= \sents -> 
    sents_to_func sents >>>= \func -> interact' func
    

(>>>=) :: (Show a) => Either a b -> ( b -> IO ()) -> IO ()
Left  a >>>= _  = do 
 hPutStrLn stderr $ show a
 hPutStrLn stderr $ "\n\nPress Enter after reading this message."
 void getLine
Right b >>>= f  = f b


interact' :: (Show a) => (String -> Either a String) -> IO ()
interact' f = do 
 s <- getContents
 forM_ (lines s) $ \line -> do
  case f line of 
   Right str -> putStrLn str
   Left a -> hPutStrLn stderr (show a) >> putStrLn ""


 




