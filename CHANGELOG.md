# Revision history for testcontainer-hs

## 0.4.0.0 -- 2023-02-20

* BREAKING: Refined lifecycle management. testcontainers is now using testcontainers/ryuk resource reaper to cleanup containers, networks and volumes. Release keys for containers are deprecated. (@alexbiehl, https://github.com/testcontainers/testcontainers-hs/pull/33)

* BREAKING: Ports can be passed as both numbers as well as strings ala "80/tcp" or "9423/udp" (@alexbiehl, https://github.com/testcontainers/testcontainers-hs/pull/31)

* BREAKING: Introduce TestContainer monad (@alexbiehl, https://github.com/testcontainers/testcontainers-hs/pull/29)

* Ability to use Docker networks (@alexbiehl, https://github.com/testcontainers/testcontainers-hs/pull/32)

* Some internal module reorganization (@alexbiehl, https://github.com/testcontainers/testcontainers-hs/pull/32)

* Refined timeout handling for WaitUntilReady (@alexbiehl, https://github.com/testcontainers/testcontainers-hs/pull/30)

* Moved repository under the [testcontainers](https://github.com/testcontainers) organization

## 0.2.0.0 -- 2020-08-07

* Dependency rework (@blackheaven, https://github.com/alexbiehl/testcontainers-hs/pull/3)

## 0.1.0.0 -- 2020-08-06

* First version. Released on an unsuspecting world.
