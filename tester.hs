{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
import System.Environment
import System.IO
import System.Process
import System.Info()
import Control.Monad
import Control.Exception as E
import Control.Monad.Reader


tell :: Bool -> String -> IO ()
tell True str = hPutStrLn stderr str
tell False _ = return ()

tell' :: String -> ReaderT Bool IO ()
tell' str = do
 bool <- ask
 lift $ tell bool str

call' :: String -> ReaderT Bool IO ()
call' str = do
 bool <- ask
 lift $ call bool str

call :: Bool -> String -> IO ()
call True str = callCommand str
call False str = callCommand (str ++ " 2> /dev/null")

main :: IO ()
main = do
 args <- getArgs
 if "--verbose" `elem` args
  then f (filter (/="--verbose") args) `runReaderT` True
  else f args `runReaderT` ("--create" `elem` args || "--createJSON" `elem` args)

f :: [String] -> ReaderT Bool IO ()
f [] = lift $ do
   hPutStrLn stderr $ unlines[
    "Usage: ",
    "\t./tester --create [sample_names]",
    "\t./tester --createJSON [sample_names]",
    "\t./tester --check [sample_names]",
    "\t./tester --checkJSON [sample_names]",
    "\t./tester --check_from [file_name]",
    "\t./tester --checkJSON_from [file_name]",
    "\t./tester --verbose (other options)",
    "Note that --create automatically adds --verbose."]
   void getLine
f ("--create":arr) = forM_ arr $ \name -> do
   tell' $ "Creating the output sample for {" ++ name ++ "}..."
   call' $ 
    "./akrantiain2 samples/sample_" ++ name ++ ".snoj < samples/input_sample_" ++ name ++ ".txt > samples/output_sample_" ++ name ++ ".txt"
   tell' $ "Created the output sample for {" ++ name ++ "}."
f ("--createJSON":arr) = forM_ arr $ \name -> do
   tell' $ "Creating the JSON dump for {" ++ name ++ "}..."
   call' $ 
    "./akrantiain2 --toJSON samples/sample_" ++ name ++ ".snoj > " ++ getJSamplePath name
   tell' $ "Created the JSON dump for {" ++ name ++ "}."
f ("--check":arr) = check arr 
f ("--checkJSON":arr) = checkJSON arr 
f ["--check_from",filename] = do
 arr <- lift $ (filter (/="") . lines) <$> readFile filename 
 check arr
f ["--checkJSON_from",filename] = do
 arr <- lift $ (filter (/="") . lines) <$> readFile filename 
 checkJSON arr

check :: [String] -> ReaderT Bool IO ()  
check arr = do
 forM_ arr $ \name -> 
  unless (null name || head name == '#') $ do
   tell' $ "Checking the output of sample {" ++ name ++ "}..."
   call' $ 
    "./akrantiain2 samples/sample_" ++ name ++ ".snoj < samples/input_sample_" ++ name ++ ".txt > samples/.output_sample_" ++ name ++ ".tmp"
   lift $ callCommand ("diff samples/.output_sample_" ++ name ++ ".tmp samples/output_sample_" ++ name ++ ".txt") `E.catch` foo name
   tell' $ "Finished checking the output of sample {" ++ name ++ "}."
 lift $ hPutStrLn stderr "Finished checking all cases."

checkJSON :: [String] -> ReaderT Bool IO ()  
checkJSON arr = do
 forM_ arr $ \name -> 
  unless (null name || head name == '#') $ do
   tell' $ "Checking the output of sample {" ++ name ++ "}..."
   call' $ 
    "./akrantiain2 --toJSON samples/sample_" ++ name ++ ".snoj > samples/.jsample_" ++ name ++ ".tmp"
   lift $ callCommand ("diff samples/.jsample_" ++ name ++ ".tmp " ++ getJSamplePath name) `E.catch` foo name
   tell' $ "Finished checking the JSON dump of sample {" ++ name ++ "}."
 lift $ hPutStrLn stderr "Finished checking all cases."

foo :: String -> E.IOException -> IO a
foo name e = do
    hPutStrLn stderr $ "FAILURE WHILE CHECKING THE OUTPUT OF SAMPLE {" ++ name ++ "}."
    throwIO e

getJSamplePath :: String -> String
getJSamplePath name = "samples/jsample/jsample_" ++ name ++ ".json"


