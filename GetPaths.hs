module GetPaths
(getJSamplePath
,getSnojPath
,getInputSamplePath
,getOutputSamplePath
,getJSampleTmpPath
,getOSampleTmpPath
) where

getJSamplePath :: String -> String
getJSamplePath name = "samples/jsample/jsample_" ++ name ++ ".json"

getSnojPath :: String -> String
getSnojPath name = "samples/sample_" ++ name ++ ".snoj"

getInputSamplePath :: String -> String
getInputSamplePath name = "samples/input_sample/input_sample_" ++ name ++ ".txt" 

getOutputSamplePath :: String -> String
getOutputSamplePath name = "samples/output_sample/output_sample_" ++ name ++ ".txt" 

getJSampleTmpPath :: String -> String
getJSampleTmpPath name = "samples/tmp/.jsample_" ++ name ++ ".tmp"

getOSampleTmpPath :: String -> String
getOSampleTmpPath name = "samples/tmp/.output_sample_" ++ name ++ ".tmp"
