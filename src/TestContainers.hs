-- |
-- This module shall be used as entrypoint to the testcontainers library. It exports
-- all the necessary types and functions for most common use-cases.
--
module TestContainers
  (

    -- * Docker images

    M.ImageTag

  , M.Image
  , M.imageTag

    -- * Referring to Docker images

  , M.ToImage
  , M.fromTag
  , M.fromBuildContext
  , M.build

  -- * @docker run@ parameters

  , M.ContainerRequest
  , M.containerRequest

  , M.setName
  , M.setCmd
  , M.setRm
  , M.setEnv
  , M.setLink
  , M.setExpose
  , M.setWaitingFor

    -- * Running Docker containers (@docker run@)

  , M.Container
  , M.containerIp
  , M.containerPort
  , M.containerReleaseKey
  , M.containerImage

  , M.run

    -- * Inspecting Docker containers

  , M.InspectOutput
  , M.inspect

    -- * Docker container lifecycle

  , M.stop
  , M.kill
  , M.rm
  , M.withLogs

    -- * Readiness checks

  , M.WaitUntilReady

    -- ** Timeout for readiness checks

  , M.waitUntilTimeout

    -- ** Waiting on particular log lines

  , M.Pipe(..)
  , M.waitWithLogs
  , M.waitForLogLine

    -- ** Wait until connection can be established

  , M.waitUntilMappedPortReachable

    -- * Monad

  , M.MonadDocker

    -- * Configuration

  , Config(..)
  , defaultDockerConfig
  , determineConfig

    -- * Exceptions

  , M.DockerException(..)
  , M.TimeoutException(..)
  , M.UnexpectedEndOfPipe(..)

    -- * Misc. Docker functions

  , dockerHostOs
  , isDockerOnLinux

    -- * Predefined Docker images

  , M.redis
  , M.mongo

    -- * Reexports

  , ResIO
  , runResourceT
  , (&)
  ) where

import           TestContainers.Docker as M
import           TestContainers.Image  as M
