.PHONY: build repl test release

PROJECT_NAME ?= $(shell grep "^name" docker.cabal | cut -d " " -f17)
VERSION ?= $(shell grep "^version:" docker.cabal | cut -d " " -f14)
RESOLVER ?= $(shell grep "^resolver:" stack.yaml | cut -d " " -f2)
GHC_VERSION ?= $(shell stack ghc -- --version | cut -d " " -f8)
ARCH=$(shell uname -m)

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
	@echo "\nAnd: \n\ngit tag ${VERSION} && git push --tags\n"

