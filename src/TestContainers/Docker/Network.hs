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
    createNetwork,
    NetworkRequest,
    defaultNetworkRequest,
    withDriver,
    withIpv6,
  )
where

import Control.Monad (replicateM)
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Resource (ReleaseKey, register)
import Data.Text (Text, pack, strip)
import qualified System.Random as Random
import TestContainers.Docker.Internal (NetworkId, docker)
import TestContainers.Monad (Config (..), TestContainer)
import Prelude hiding (id)

-- | Handle to a Docker network.
--
-- @since x.x.x
data Network = Network
  { id :: NetworkId,
    releaseKey :: ReleaseKey
  }

-- | Returns the id of the network.
--
-- @since x.x.x
networkId :: Network -> NetworkId
networkId Network {id} = id

-- | Parameters for creating a new Docker network.
--
-- @since x.x.x
data NetworkRequest = NetworkRequest
  { ipv6 :: Bool,
    driver :: Maybe Text
  }

-- | Default parameters for creating a new Docker network.
--
-- @since x.x.x
defaultNetworkRequest :: NetworkRequest
defaultNetworkRequest =
  NetworkRequest
    { ipv6 = False,
      driver = Nothing
    }

-- | Enable IPv6 for the Docker network.
--
-- @since x.x.x
withIpv6 :: NetworkRequest -> NetworkRequest
withIpv6 request =
  request {ipv6 = True}

-- | Driver to manage the Network (default "bridge").
--
-- @since x.x.x
withDriver :: Text -> NetworkRequest -> NetworkRequest
withDriver driver request =
  request {driver = Just driver}

-- | Creates a new 'Network' from a 'NetworkRequest'.
--
-- @since x.x.x
createNetwork :: NetworkRequest -> TestContainer Network
createNetwork NetworkRequest {..} = do
  Config {..} <- ask

  name <-
    pack <$> replicateM 16 (Random.randomRIO ('a', 'z'))

  stdout <-
    docker configTracer $
      concat $
        [["network", "create"]]
          ++ [["--driver", driver_] | Just driver_ <- [driver]]
          ++ [["--ipv6" | ipv6]]
          ++ [[name]]

  let id :: NetworkId
      !id =
        -- N.B. Force to not leak STDOUT String
        strip (pack stdout)

  releaseKey <- register $ do
    _stdout <- docker configTracer ["network", "rm", id]
    pure ()

  pure Network {..}
