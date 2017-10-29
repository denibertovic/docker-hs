# An API client for docker written in Haskell

| Master |
| -------|
| [![master](https://travis-ci.org/denibertovic/docker-hs.svg?branch=master)](https://travis-ci.org/denibertovic/docker-hs) |


## Current state

Supported Docker Engine Api version: `v1.24` and onwards.

Anything upward of that should work since Docker versions their API.
Older docker version and engine api versions are not supported at the moment.

## Documentation

The API-documentation is available at
[Hackage](https://hackage.haskell.org/package/docker). There are also some
usage-examples in the main library source file,
[`Client.hs`](https://hackage.haskell.org/package/docker/docs/Docker-Client.html).

## Contributing

If you wish to contribute please see any issue tagged with "help wanted".
Make sure to submit your PR's against the `master` branch.

Please consider filling an Issue first and discuss design decisions and implementation details before
writing any code. This is so that no development cycles go to waste on implementing a feature that
might not get merged either because of implementation details or other reasons.


## IRC

If you have any questions or suggestions you can find the maintainers in `#docker-haskell`
on freenode.
