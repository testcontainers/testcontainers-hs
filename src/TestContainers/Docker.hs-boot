module TestContainers.Docker
  ( createRyukReaper,
  )
where

import TestContainers.Docker.Reaper (Reaper)
import TestContainers.Monad (TestContainer)

createRyukReaper :: TestContainer Reaper
