.PHONY: test build repl release

VERSION ?= $(shell grep "^version:" docker.cabal | cut -d " " -f14)

build:
	@stack build

repl:
	@stack repl

test:
	@stack test

release:
	@stack build --haddock
	@stack sdist
	@echo "\n\nDone. Now run: \n\nstack upload <path_to_tarball>\n"

