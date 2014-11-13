batch-beautifier: dist
	cabal build
	ln -sf dist/build/batch-beautifier/batch-beautifier batch-beautifier

test: dist
	cabal build
	cabal test

dist:
	cabal configure --enable-tests

clean:
	cabal clean

.PHONY: batch-beautifier clean test
