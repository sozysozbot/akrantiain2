@echo off
del *.exe
cabal update
cabal install unicode-transforms
ghc --make akrantiain2
ghc --make debug1
ghc --make debug2
ghc --make debug3
pause
exit /b
