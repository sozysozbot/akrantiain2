.PHONY: foo clean debug
foo :
	ghc --version || sudo apt-get install haskell-platform
	cabal update || true
	cabal install unicode-transforms || true
	ghc --make akrantiain2.hs -o akrantiain2
	chmod 755 akrantiain2

debug :
	ghc --make akrantiain2.hs -o akrantiain2
	ghc --make test/debug1.hs -o test/debug1
	ghc --make test/debug2.hs -o test/debug2
	ghc --make test/tester.hs -o test/tester
	chmod 755 akrantiain2
	chmod 755 test/debug1
	chmod 755 test/debug2
	chmod 755 test/tester
	./test/debug1 > test/gerhw1.txt
	diff test/gerhw1.txt test/debug1_res.txt
#	./test/debug2 > test/gerhw2.txt
#	diff test/gerhw2.txt test/debug2_res.txt
	./test/tester --check_from test/testerlist.txt
	./test/tester --checkJSON_from test/testerlist2.txt


clean:
	$(RM) akrantiain2
	$(RM) test/debug1
	$(RM) test/debug2
	$(RM) test/tester


document:
	scss manuals/introduction/help.scss manuals/introduction/help.css
