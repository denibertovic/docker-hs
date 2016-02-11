.PHONY: test build upload dist configure

VERSION ?= $(shell grep "^version:" docker.cabal | cut -d " " -f14)

build:
	@stack build

test:
	@stack test

sdist:
	@stack sdist

upload: dist
	@stack upload .stack-work/dist/x86_64-linux/Cabal-1.18.1.5/docker-${VERSION}.tar.gz

