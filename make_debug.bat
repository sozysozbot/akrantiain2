@echo off
del *.exe
cabal update
cabal install unicode-transforms
ghc --make akrantiain2
ghc --make debug1
ghc --make debug2
ghc --make debug3
ghc --make test\tester
test\tester --check_from test\testerlist.txt
test\tester --checkJSON_from test\testerlist2.txt
pause
exit /b
