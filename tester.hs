import System.Environment
import System.IO
import System.Process
import System.Info
import Control.Monad
import Control.Exception as E


tell True = hPutStrLn stderr
tell False = \_ -> return ()

call True str = callCommand str
call False str = callCommand (str ++ " 2> /dev/null")

main :: IO ()
main = do
 args <- getArgs
 f args

f [] = do
   hPutStrLn stderr $ unlines[
    "Usage: ",
    "\t./tester --create [sample_names]",
    "\t./tester --check [sample_names]",
    "\t./tester --check_from [file_name]"]
   void getLine
f ("--create":arr) = forM_ arr $ \name -> do
   tell True $ "Creating the output sample for {" ++ name ++ "}..."
   call True $ 
    "./akrantiain2 samples/sample_" ++ name ++ ".snoj < samples/input_sample_" ++ name ++ ".txt > samples/output_sample_" ++ name ++ ".txt"
   tell True $ "Created the output sample for {" ++ name ++ "}."
f ("--check":arr) = check arr
f ["--check_from",filename] = do
   arr <- (filter (/="") . lines) <$> readFile filename 
   check arr
   
check arr = forM_ arr $ \name -> do
   tell True $ "Checking the output of sample {" ++ name ++ "}..."
   call True $ 
    "./akrantiain2 samples/sample_" ++ name ++ ".snoj < samples/input_sample_" ++ name ++ ".txt > samples/.output_sample_" ++ name ++ ".tmp"
   call True ("diff samples/.output_sample_" ++ name ++ ".tmp samples/output_sample_" ++ name ++ ".txt") `E.catch` foo name
   tell True $ "Finished checking the output of sample {" ++ name ++ "}."

foo :: String -> E.IOException -> IO a
foo name e = do
    hPutStrLn stderr $ "FAILURE WHILE CHECKING THE OUTPUT OF SAMPLE {" ++ name ++ "}."
    throwIO e
