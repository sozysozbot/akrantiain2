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
	ghc --make test/tester.hs -o test/tester
	chmod 755 akrantiain2
	chmod 755 debug1
	chmod 755 debug2
	chmod 755 test/tester
	./debug1 > test/gerhw1.txt
	diff test/gerhw1.txt test/debug1_res.txt
#	./debug2 > test/gerhw2.txt
#	diff test/gerhw2.txt test/debug2_res.txt
	./test/tester --check_from test/testerlist.txt
	./test/tester --checkJSON_from test/testerlist2.txt


clean:
	$(RM) akrantiain2
	$(RM) debug1
	$(RM) debug2
	$(RM) test/tester


document:
	scss manuals/introduction/help.scss manuals/introduction/help.css
