default: test

build:
	dune build @all

test:
	dune runtest

clean:
	dune clean

.PHONY: default build test clean
