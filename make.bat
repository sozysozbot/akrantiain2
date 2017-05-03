@echo off
del *.exe
cabal update
cabal install unicode-transforms
ghc --make akrantiain2
pause
exit /b
