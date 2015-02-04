batch-beautifier: dist
	cabal build
	ln -sf dist/build/batch-beautifier/batch-beautifier batch-beautifier

test: dist
	cabal test

dist:
	cabal configure --enable-tests

clean:
	cabal clean

update:
	script/update.sh

.PHONY: batch-beautifier clean test
