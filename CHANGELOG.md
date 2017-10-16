# CHANGELOG

## 0.4.1.1 (2017-10-16)

- Small change to add env var for skipping integration tests.
  This is mostly to avoid running those on stackage.

## 0.4.1.0 (2017-10-04)

- Relaxed aeson, tls and time upper bounds


## 0.4.0.2 (2017-09-19)

- Support LTS 9.4

## 0.4.0.1 (2017-06-29)

- fixes omKillDisable parsing from json
- adds Entrypoint data type and tests edge cases
- more tests
- small comsmetic changes

## 0.4.0.0 (2017-03-20)

- Adds streaming support via conduits (changes API a bit hence the major version release)
- Added support for streaming log output
- Added support for pulling images from the hub (and streaming the output)

## 0.3.0.1 (2016-09-07)

- Small cosmetic changes and marking OVERLAPPING instance

## 0.3.0.0 (2016-09-06)

Major rewrite

## 0.2.0.4 (2016-02-11)

- Adds function for getting all containers and images

## 0.2.0.3 (2015-10-22)

- Adds SSL support for talking to the docker daemon

