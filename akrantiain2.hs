{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
import Prelude hiding (undefined)
import Akrantiain.Lexer
import Text.Parsec
import System.Environment
import System.IO
import Akrantiain.Sents_to_rules
import Control.Monad(forM_, when)
import System.Process
import System.Info



main :: IO ()
main = do
 args <- getArgs
 when (os == "mingw32") $ callCommand "chcp 65001 > nul"
 case args of
  []    -> putStrLn "mi'e .akranti'ain."
  (fname:_) -> do
   handle <- openFile fname ReadMode 
   hSetEncoding handle utf8
   input <- hGetContents handle
   runParser sentences () fname input >>>= \sents -> 
    sents_to_func sents >>>= \func -> interact' func
    

(>>>=) :: (Show a) => Either a b -> ( b -> IO ()) -> IO ()
Left  a >>>= _  = hPutStrLn stderr $ show a
Right b >>>= f  = f b


interact' :: (Show a) => (String -> Either a String) -> IO ()
interact' f = do 
 s <- getContents
 forM_ (lines s) $ \line -> do
  case f line of 
   Right str -> putStrLn str
   Left a -> hPutStrLn stderr (show a) >> putStrLn ""


 




