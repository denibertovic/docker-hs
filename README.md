# An API client for docker written in Haskell

| Master |
| -------|
| [![master](https://travis-ci.org/denibertovic/docker-hs.svg?branch=master)](https://travis-ci.org/denibertovic/docker-hs) |


## Current state

Supported Docker Engine Api version: `v1.24` and onwards.

Anything upward of that should work since Docker versions their API.
Older docker version and engine api versions are not supported at the moment.

## Contributing

Please see [CONTRIBUTING.md](CONTRIBUTING.md).

### Project Setup

For working on the library, you need the Haskell Tool Stack installed (see [the
Haskell Tool Stack
website](https://docs.haskellstack.org/en/stable/install_and_upgrade/)). You
also need `make` to use the `Makefile` included in the project. Run `make help`
to see the available commands (for building, running the tests and releasing).

## IRC

If you have any questions or suggestions you can find the maintainers in `#docker-haskell`
on freenode.
