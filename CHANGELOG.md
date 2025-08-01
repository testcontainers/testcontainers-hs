# Revision history for testcontainer-hs

## 0.5.2.0 -- Unreleased

* Introduce `withCopyFileToContainer` to copy local files to the container (@LaurentRDC, https://github.com/testcontainers/testcontainers-hs/pull/62)

## 0.5.1.0 -- 2025-01-14

* Introduce `withWorkingDirectory` to set the working directory inside a container (@alexbiehl, https://github.com/testcontainers/testcontainers-hs/pull/37)

* Add `fromExistingNetwork` to allow connecting containers to existing, unmanaged networks (@alexbiehl, https://github.com/testcontainers/testcontainers-hs/pull/41)

* Add `setMemory` to limit the memory of a Docker container;

* Add `setCpus` to limit the CPUs limit of a docker container;

* Add `fromImageId` to get an `Image` from an image id;

## 0.5.0.0 -- 2023-02-20

* BREAKING: Refined lifecycle management. testcontainers is now using testcontainers/ryuk resource reaper to cleanup containers, networks and volumes. Release keys for containers are deprecated. (@alexbiehl, https://github.com/testcontainers/testcontainers-hs/pull/33)

* BREAKING: Ports can be passed as both numbers as well as strings ala "80/tcp" or "9423/udp" (@alexbiehl, https://github.com/testcontainers/testcontainers-hs/pull/31)

* BREAKING: Introduce TestContainer monad (@alexbiehl, https://github.com/testcontainers/testcontainers-hs/pull/29)

* Control container naming (@blackheaven, https://github.com/testcontainers/testcontainers-hs/pull/18)

* Ability to use Docker networks (@alexbiehl, https://github.com/testcontainers/testcontainers-hs/pull/32)

* Better handling of running testcontainers from within Docker (@michivi, https://github.com/testcontainers/testcontainers-hs/pull/22)

* Ability to wait for particular HTTP routes to become available with waitforHttp (@michivi, https://github.com/testcontainers/testcontainers-hs/pull/24)

* Some internal module reorganization (@alexbiehl, https://github.com/testcontainers/testcontainers-hs/pull/32)

* Refined timeout handling for WaitUntilReady (@alexbiehl, https://github.com/testcontainers/testcontainers-hs/pull/30)

* Moved repository under the [testcontainers](https://github.com/testcontainers) organization

## 0.2.0.0 -- 2020-08-07

* Dependency rework (@blackheaven, https://github.com/alexbiehl/testcontainers-hs/pull/3)

## 0.1.0.0 -- 2020-08-06

* First version. Released on an unsuspecting world.
