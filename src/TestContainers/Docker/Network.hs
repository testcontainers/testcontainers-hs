{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TestContainers.Docker.Network
  ( -- * Network
    NetworkId,
    Network,
    networkId,

    -- * Creating networks
    fromExistingNetwork,
    createNetwork,
    NetworkRequest,
    networkRequest,
    withDriver,
    withIpv6,
    withoutReaper,
  )
where

import Control.Monad (replicateM)
import Control.Monad.Reader (ask)
import Data.Text (Text, pack, strip)
import qualified System.Random as Random
import TestContainers.Docker.Internal (NetworkId, WithoutReaper (..), docker)
import TestContainers.Docker.Reaper (reaperLabels)
import TestContainers.Monad (Config (..), TestContainer)
import Prelude hiding (id)

-- | Handle to a Docker network.
--
-- @since 0.5.0.0
newtype Network = Network
  { id :: NetworkId
  }

-- | Returns the id of the network.
--
-- @since 0.5.0.0
networkId :: Network -> NetworkId
networkId Network {id} = id

-- | Parameters for creating a new Docker network.
--
-- @since 0.5.0.0
data NetworkRequest = NetworkRequest
  { ipv6 :: Bool,
    driver :: Maybe Text,
    labels :: [(Text, Text)],
    noReaper :: Bool
  }

instance WithoutReaper NetworkRequest where
  withoutReaper request = request {noReaper = True}

-- | Default parameters for creating a new Docker network.
--
-- @since 0.5.0.0
networkRequest :: NetworkRequest
networkRequest =
  NetworkRequest
    { ipv6 = False,
      driver = Nothing,
      labels = [],
      noReaper = False
    }

-- | Enable IPv6 for the Docker network.
--
-- @since 0.5.0.0
withIpv6 :: NetworkRequest -> NetworkRequest
withIpv6 request =
  request {ipv6 = True}

-- | Driver to manage the Network (default "bridge").
--
-- @since 0.5.0.0
withDriver :: Text -> NetworkRequest -> NetworkRequest
withDriver driver request =
  request {driver = Just driver}

-- | Creates a 'Network' from an existing 'NetworkId'. Note that the 'Network' is
-- not managed by the 'TestContainer' monad and as such is not being cleaned up
-- afterwards.
--
-- @since x.x.x
fromExistingNetwork :: NetworkId -> TestContainer Network
fromExistingNetwork id =
  pure Network {id}

-- | Creates a new 'Network' from a 'NetworkRequest'.
--
-- @since 0.5.0.0
createNetwork :: NetworkRequest -> TestContainer Network
createNetwork NetworkRequest {..} = do
  Config {..} <- ask

  name <-
    pack <$> replicateM 16 (Random.randomRIO ('a', 'z'))

  reaper <-
    configCreateReaper

  -- Creating the network with the reaper labels ensures cleanup
  -- at the end of the session
  let additionalLabels =
        if noReaper then [] else reaperLabels reaper

  stdout <-
    docker configTracer $
      concat $
        [["network", "create"]]
          ++ [["--driver", driver_] | Just driver_ <- [driver]]
          ++ [["--ipv6" | ipv6]]
          ++ [["--label", label <> "=" <> value] | (label, value) <- additionalLabels <> labels]
          ++ [[name]]

  let id :: NetworkId
      !id =
        -- N.B. Force to not leak STDOUT String
        strip (pack stdout)

  pure Network {..}
