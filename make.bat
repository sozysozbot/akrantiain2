@echo off
del *.exe
cabal update
cabal install unicode-transforms
cabal install aeson
ghc --make akrantiain2
pause
exit /b
