{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module TestContainers.Docker.Reaper
  ( Reaper,
    reaperLabels,

    -- * Ryuk based reaper
    ryukImageTag,
    ryukPort,
    newRyukReaper,
  )
where

import Control.Monad (replicateM)
import Control.Monad.Trans.Resource (MonadResource, allocate)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as Socket
import qualified System.Random as Random

-- | Reaper for safe resource cleanup.
--
-- @since x.x.x
data Reaper = Reaper
  { -- | @runReaper label value@ reaps Docker any Docker resource with a matching
    -- label.
    runReaper :: Text -> Text -> IO (),
    -- | Additional labels to add to any Docker resource on creation. Adding the
    -- labels is necessary in order for the 'Reaper' to find resources for cleanup.
    labels :: [(Text, Text)]
  }

-- | Additional labels to add to any Docker resource on creation. Adding the
-- labels is necessary in order for the 'Reaper' to find resources for cleanup.
--
-- @since x.x.x
reaperLabels :: Reaper -> [(Text, Text)]
reaperLabels Reaper {labels} =
  labels

-- | Ryuk based resource reaper
--
-- @since x.x.x
newtype Ryuk = Ryuk {ryukSocket :: Socket.Socket}

-- | Tag for the ryuk image
--
-- @since x.x.x
ryukImageTag :: Text
ryukImageTag =
  "docker.io/testcontainers/ryuk:0.3.4"

-- | Exposed port for the ryuk reaper.
--
-- @since x.x.x
ryukPort :: (Num a) => a
ryukPort =
  8080

-- | Creates a new 'Reaper' from a host and port.
--
-- @since x.x.x
newRyukReaper ::
  (MonadResource m) =>
  -- | Host
  Text ->
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
            head <$> Socket.getAddrInfo (Just hints) (Just (unpack host)) (Just (show port))
          socket <-
            Socket.openSocket address
          Socket.connect socket (Socket.addrAddress address)
          pure (socket, runRyuk sessionId (Ryuk socket))
      )
      ( \(socket, ryuk) -> do
          runReaper ryuk sessionIdLabel sessionId
          Socket.close socket
      )

  pure ryuk

runRyuk ::
  -- | Session id
  Text ->
  Ryuk ->
  Reaper
runRyuk sessionId ryuk =
  Reaper
    { runReaper = \label value -> do
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
