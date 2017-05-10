import System.Environment
import System.IO
import System.Process
import System.Info
import Control.Monad



main :: IO ()
main = do
 args <- getArgs
 case args of 
  [] -> do
   hPutStrLn stderr "Usage: ./tester create [sample_names]"
   void getLine
  ("create":arr) -> forM_ arr $ \name -> do
   hPutStrLn stderr $ "Creating output sample for {" ++ name ++ "}..."
   callCommand $ 
    "./akrantiain2 samples/sample_" ++ name ++ ".snoj < samples/input_sample_" ++ name ++ ".txt > samples/output_sample_" ++ name ++ ".txt"
   hPutStrLn stderr $ "Created output sample for {" ++ name ++ "}."
