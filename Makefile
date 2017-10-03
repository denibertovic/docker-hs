.PHONY: build repl test release help

.DEFAULT_GOAL = help

PROJECT_NAME ?= $(shell grep "^name" docker.cabal | cut -d " " -f17)
VERSION ?= $(shell grep "^version:" docker.cabal | cut -d " " -f14)
RESOLVER ?= $(shell grep "^resolver:" stack.yaml | cut -d " " -f2)
GHC_VERSION ?= $(shell stack ghc -- --version | cut -d " " -f8)
ARCH=$(shell uname -m)

## Run build
build:
	@stack build

## Run repl
repl:
	@stack repl

## Run tests
test:
	@stack test

## Cut new release
release:
	@stack build --haddock
	@stack sdist
	@echo "\n\nDone. Now run: \n\nstack upload <path_to_tarball>\n"
	@echo "\nAnd: \n\ngit tag ${VERSION} && git push --tags\n"

## Show help screen.
help:
	@echo "Please use \`make <target>' where <target> is one of\n\n"
	@awk '/^[a-zA-Z\-\_0-9]+:/ { \
		helpMessage = match(lastLine, /^## (.*)/); \
		if (helpMessage) { \
			helpCommand = substr($$1, 0, index($$1, ":")-1); \
			helpMessage = substr(lastLine, RSTART + 3, RLENGTH); \
			printf "%-30s %s\n", helpCommand, helpMessage; \
		} \
	} \
	{ lastLine = $$0 }' $(MAKEFILE_LIST)

