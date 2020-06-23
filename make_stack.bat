@echo off
del *.exe
stack install unicode-transforms
stack install aeson
stack ghc akrantiain2
pause
exit /b
