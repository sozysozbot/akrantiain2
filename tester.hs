import System.Environment
import System.IO
import System.Process
import System.Info
import Control.Monad
import Control.Exception as E


main :: IO ()
main = do
 args <- getArgs
 case args of 
  [] -> do
   hPutStrLn stderr $ unlines[
    "Usage: ",
    "\t./tester create [sample_names]",
    "\t./tester check [sample_names]",
    "\t./tester check_from [file_name]"]
   void getLine
  ("create":arr) -> forM_ arr $ \name -> do
   hPutStrLn stderr $ "Creating the output sample for {" ++ name ++ "}..."
   callCommand $ 
    "./akrantiain2 samples/sample_" ++ name ++ ".snoj < samples/input_sample_" ++ name ++ ".txt > samples/output_sample_" ++ name ++ ".txt"
   hPutStrLn stderr $ "Created the output sample for {" ++ name ++ "}."
  ("check":arr) -> action arr
  ["check_from",filename] -> do
   arr <- (filter (/="") . lines) <$> readFile filename 
   action arr
   
action arr = forM_ arr $ \name -> do
   hPutStrLn stderr $ "Checking the output of sample {" ++ name ++ "}..."
   callCommand $ 
    "./akrantiain2 samples/sample_" ++ name ++ ".snoj < samples/input_sample_" ++ name ++ ".txt > samples/.output_sample_" ++ name ++ ".tmp"
   callCommand ("diff samples/.output_sample_" ++ name ++ ".tmp samples/output_sample_" ++ name ++ ".txt") `E.catch` foo name
   hPutStrLn stderr $ "Finished checking the output of sample {" ++ name ++ "}."

foo :: String -> E.IOException -> IO a
foo name e = do
    hPutStrLn stderr $ "FAILURE WHILE CHECKING THE OUTPUT OF SAMPLE {" ++ name ++ "}."
    throwIO e
