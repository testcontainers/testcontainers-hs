{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module TestContainers.Docker.Reaper
  ( Reaper (..),
    reaperLabels,

    -- * Ryuk based reaper
    ryukImageTag,
    ryukPort,
    newRyukReaper,
  )
where

import Control.Monad (replicateM)
import Control.Monad.Trans.Resource (MonadResource, allocate)
import Data.IP (IP)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as Socket
import qualified System.Random as Random

-- | Reaper for safe resource cleanup. This type is exposed to allow users to
-- create their own 'Reapers'.
--
-- @since 0.5.0.0
data Reaper = Reaper
  { -- | Registers a @label = value@ pair for reaping. Reaping happens when
    -- closing/de-allocating of the 'Reaper' through 'MonadResource'.
    register ::
      -- \| Label
      Text ->
      -- \| Value
      Text ->
      IO (),
    -- | Additional labels to add to any Docker resource on creation. Adding the
    -- labels is necessary in order for the 'Reaper' to find resources for cleanup.
    labels :: [(Text, Text)]
  }

-- | Additional labels to add to any Docker resource on creation. Adding the
-- labels is necessary in order for the 'Reaper' to find resources for cleanup.
--
-- @since 0.5.0.0
reaperLabels :: Reaper -> [(Text, Text)]
reaperLabels Reaper {labels} =
  labels

-- | Ryuk based resource reaper
--
-- @since 0.5.0.0
newtype Ryuk = Ryuk {ryukSocket :: Socket.Socket}

-- | Tag for the ryuk image
--
-- @since 0.5.0.0
ryukImageTag :: Text
ryukImageTag =
  "docker.io/testcontainers/ryuk:0.3.4"

-- | Exposed port for the ryuk reaper.
--
-- @since 0.5.0.0
ryukPort :: (Num a) => a
ryukPort =
  8080

-- | Creates a new 'Reaper' from a host and port.
--
-- @since 0.5.0.0
newRyukReaper ::
  (MonadResource m) =>
  -- | Host
  IP ->
  -- | Port
  Int ->
  m Reaper
newRyukReaper host port = do
  sessionId <-
    pack <$> replicateM 16 (Random.randomRIO ('a', 'z'))

  (_releaseKey, (_socket, ryuk)) <-
    allocate
      ( do
          let hints =
                Socket.defaultHints {Socket.addrSocketType = Socket.Stream}
          address <-
            head <$> Socket.getAddrInfo (Just hints) (Just (show host)) (Just (show port))
          socket <-
            Socket.socket
              (Socket.addrFamily address)
              (Socket.addrSocketType address)
              (Socket.addrProtocol address)
          Socket.connect socket (Socket.addrAddress address)

          -- Construct the reaper and regiter the session with it.
          -- Doing it here intead of in the teardown (like we did before)
          -- guarantees the Reaper knows about our session.
          let reaper =
                newReaper sessionId (Ryuk socket)
          register reaper sessionIdLabel sessionId

          pure (socket, reaper)
      )
      ( \(socket, _ryuk) -> do
          -- Tearing down the connection lets Ryuk know it can reap the
          -- running containers.
          Socket.close socket
      )

  pure ryuk

newReaper ::
  -- | Session id
  Text ->
  Ryuk ->
  Reaper
newReaper sessionId ryuk =
  Reaper
    { register = \label value -> do
        Socket.sendAll
          (ryukSocket ryuk)
          ("label=" <> encodeUtf8 label <> "=" <> encodeUtf8 value <> "\n")
        _ <- Socket.recv (ryukSocket ryuk) 2
        pure (),
      labels =
        [ (sessionIdLabel, sessionId)
        ]
    }

sessionIdLabel :: Text
sessionIdLabel = "org.testcontainers.haskell.session"
