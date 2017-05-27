@echo off
del *.exe
cabal update
cabal install unicode-transforms
ghc --make akrantiain2
ghc --make debug1
ghc --make debug2
ghc --make debug3
ghc --make tester
tester check lineparine lineparine2 zyepheng emptystring1
pause
exit /b
