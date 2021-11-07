# CHANGELOG

## 0.6.0.6 (2021-11-07)

- Bump http-client version.

## 0.6.0.5 (2021-01-24)

- Bug fix for PortBinding's FromJSON instance.

## 0.6.0.4 (2020-04-01)

- Bug fix for Bind's ToJSON instance.

## 0.6.0.3 (2020-02-17)

- Support for ghc 8.8.2.
- Bump dependency version. 
- Use MonadFail for later versions.

## 0.6.0.1 (2019-05-23)

- Support ghc 8.6.4.
- Bump containers version.

## 0.6.0.0 (2018-06-28)

- Rename DeleteOpts to ContainerDeleteOpts.
- Implement image deletion.
- Bump aeson version.
- Implement network aliases.
- Implement creating/deleting networks.

## 0.5.1.1 (2018-03-09)

- Bump aeson version.

## 0.5.1.0 (2018-02-23)

- Support http-conduit-2.3.0.
- Bump http-types versions.

## 0.5.0.1 (2018-01-26)

- Bumps upper bound for http-types (properly this time).

## 0.5.0.0 (2018-01-24)

- Added (some) contribution and general docs
- Fix case when stopping non-existent container
- Added more integration tests
- Fixed error while parsing list container endpoint output
- Changed State/Status data type (related to the above fix)
- Added tests against different docker version to travis CIA
- Fixed EnvVar JSON encoding
- Fixed NetworkMode parsing
- Allow http-types 0.11

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

