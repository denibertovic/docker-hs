.PHONY: test build upload dist configure

VERSION ?= $(shell grep "^version:" docker.cabal | cut -d " " -f14)

configure:
	cabal configure --enable-tests

build: configure
	cabal build

test: build
	cabal test --show-details=always

dist: build
	cabal sdist

upload: dist
	cabal upload dist/docker-$(VERSION).tar.gz

