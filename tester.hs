{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
import System.Environment
import System.IO
import System.Process
import System.Info()
import Control.Monad
import Control.Exception as E
import Control.Monad.Reader
import Data.List(intercalate)


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
   callAkrantiain (getSnojPath name, getInputSamplePath name, getOutputSamplePath name)
   tell' $ "Created the output sample for {" ++ name ++ "}."
f ("--createJSON":arr) = forM_ arr $ \name -> do
   tell' $ "Creating the JSON dump for {" ++ name ++ "}..."
   callAkrantiainJSON (getSnojPath name, getJSamplePath name)
   tell' $ "Created the JSON dump for {" ++ name ++ "}."
f ("--check":arr) = check arr 
f ("--checkJSON":arr) = checkJSON arr 
f ["--check_from",filename] = do
 arr <- lift $ (filter (/="") . lines) <$> readFile filename 
 check arr
f ["--checkJSON_from",filename] = do
 arr <- lift $ (filter (/="") . lines) <$> readFile filename 
 checkJSON arr

callAkrantiain :: (String, String, String) -> ReaderT Bool IO ()
callAkrantiain (snoj, inputPath, outputPath) = 
  call' $ concat' ["./akrantiain2", snoj, "<", inputPath, ">", outputPath] 

callAkrantiainJSON :: (String, String) -> ReaderT Bool IO ()
callAkrantiainJSON (snoj, jsonOPath) = 
 call' $ concat' ["./akrantiain2","--toJSON", snoj, ">", jsonOPath]

check :: [String] -> ReaderT Bool IO ()  
check arr = do
 forM_ arr $ \name -> 
  unless (null name || head name == '#') $ do
   tell' $ "Checking the output of sample {" ++ name ++ "}..."
   callAkrantiain (getSnojPath name, getInputSamplePath name, getOSampleTmpPath name)
   lift $ callCommand (concat' ["diff", getOSampleTmpPath name, getOutputSamplePath name]) `E.catch` foo name
   tell' $ "Finished checking the output of sample {" ++ name ++ "}."
 lift $ hPutStrLn stderr "Finished checking all cases."

checkJSON :: [String] -> ReaderT Bool IO ()  
checkJSON arr = do
 forM_ arr $ \name -> 
  unless (null name || head name == '#') $ do
   tell' $ "Checking the JSON dump of sample {" ++ name ++ "}..."
   callAkrantiainJSON (getSnojPath name, getJSampleTmpPath name)
   lift $ callCommand (concat' ["diff", getJSampleTmpPath name, getJSamplePath name]) `E.catch` foo name
   tell' $ "Finished checking the JSON dump of sample {" ++ name ++ "}."
 lift $ hPutStrLn stderr "Finished checking all cases."

concat' :: [String] -> String
concat' = intercalate " "

foo :: String -> E.IOException -> IO a
foo name e = do
    hPutStrLn stderr $ "FAILURE WHILE CHECKING THE OUTPUT OF SAMPLE {" ++ name ++ "}."
    throwIO e

getJSamplePath :: String -> String
getJSamplePath name = "samples/jsample/jsample_" ++ name ++ ".json"

getSnojPath :: String -> String
getSnojPath name = "samples/sample_" ++ name ++ ".snoj"

getInputSamplePath :: String -> String
getInputSamplePath name = "samples/input_sample_" ++ name ++ ".txt" 

getOutputSamplePath :: String -> String
getOutputSamplePath name = "samples/output_sample_" ++ name ++ ".txt" 

getJSampleTmpPath :: String -> String
getJSampleTmpPath name = "samples/.jsample_" ++ name ++ ".tmp"

getOSampleTmpPath :: String -> String
getOSampleTmpPath name = "samples/.output_sample_" ++ name ++ ".tmp"
