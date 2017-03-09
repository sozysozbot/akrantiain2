.PHONY: foo clean
foo : 
	ghc --version || sudo apt-get install haskell-platform
	ghc --make akrantiain2.hs -o akrantiain2
	chmod 755 akrantiain2

clean:
	$(RM) akrantiain2
