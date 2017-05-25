.PHONY: foo clean debug
foo :
	ghc --version || sudo apt-get install haskell-platform
	cabal update || true
	cabal install unicode-transforms || true
	ghc --make akrantiain2.hs -o akrantiain2
	chmod 755 akrantiain2

debug :
	ghc --make akrantiain2.hs -o akrantiain2
	ghc --make debug1.hs -o debug1
	ghc --make debug2.hs -o debug2
	ghc --make debug3.hs -o debug3
	ghc --make tester.hs -o tester
	chmod 755 akrantiain2
	chmod 755 debug1
	chmod 755 debug2
	chmod 755 debug3
	chmod 755 tester
	./tester --check_from testerlist.txt

clean:
	$(RM) akrantiain2
	$(RM) debug1
	$(RM) debug2
	$(RM) debug3
	$(RM) tester
