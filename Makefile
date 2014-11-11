test dist:
	cabal build
	cabal test

dist:
	cabal configure --enable-tests

clean:
	cabal clean

.PHONY: clean test
