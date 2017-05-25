import System.Environment
import System.IO
import System.Process
import System.Info
import Control.Monad
import Control.Exception as E
import Control.Monad.Reader


tell True str = hPutStrLn stderr str
tell False str = return ()

call True str = callCommand str
call False str = callCommand (str ++ " 2> /dev/null")

main :: IO ()
main = do
 args <- getArgs
 if "--verbose" `elem` args
  then f (filter (/="--verbose") args) `runReaderT` True
  else f args `runReaderT` False

f :: [String] -> ReaderT Bool IO ()
f [] = lift $ do
   hPutStrLn stderr $ unlines[
    "Usage: ",
    "\t./tester --create [sample_names]",
    "\t./tester --check [sample_names]",
    "\t./tester --check_from [file_name]"]
   void getLine
f ("--create":arr) = do
 bool <- ask
 forM_ arr $ \name -> lift $ do
   tell bool $ "Creating the output sample for {" ++ name ++ "}..."
   call bool $ 
    "./akrantiain2 samples/sample_" ++ name ++ ".snoj < samples/input_sample_" ++ name ++ ".txt > samples/output_sample_" ++ name ++ ".txt"
   tell bool $ "Created the output sample for {" ++ name ++ "}."
f ("--check":arr) = check arr 
f ["--check_from",filename] = ReaderT $ \bool -> do
   arr <- (filter (/="") . lines) <$> readFile filename 
   check arr `runReaderT` bool

check :: [String] -> ReaderT Bool IO ()  
check arr = do
 bool <- ask
 forM_ arr $ \name -> lift $ do
   tell bool $ "Checking the output of sample {" ++ name ++ "}..."
   call bool $ 
    "./akrantiain2 samples/sample_" ++ name ++ ".snoj < samples/input_sample_" ++ name ++ ".txt > samples/.output_sample_" ++ name ++ ".tmp"
   call bool ("diff samples/.output_sample_" ++ name ++ ".tmp samples/output_sample_" ++ name ++ ".txt") `E.catch` foo name
   tell bool $ "Finished checking the output of sample {" ++ name ++ "}."

foo :: String -> E.IOException -> IO a
foo name e = do
    hPutStrLn stderr $ "FAILURE WHILE CHECKING THE OUTPUT OF SAMPLE {" ++ name ++ "}."
    throwIO e
