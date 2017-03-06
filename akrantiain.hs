{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
import Akrantiain.Lexer
import Text.Parsec
import System.Environment
import System.IO
import Akrantiain.Sents_to_func
import Control.Monad(forM_)

main :: IO ()
main = do
 args <- getArgs
 case args of
  []    -> putStrLn "mi'e .akrantiain."
  (fname:_) -> do
   handle <- openFile fname ReadMode 
   hSetEncoding handle utf8
   input <- hGetContents handle
   runParser sentences () fname input >>>= \sents -> print sents
    -- sents_to_func sents >>>= \func -> interact' func
    

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


 




