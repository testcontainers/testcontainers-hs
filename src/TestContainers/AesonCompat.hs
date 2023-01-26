{-# LANGUAGE CPP #-}

module TestContainers.AesonCompat
  ( toKey,
  )
where

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key as Key
#else
#endif
import qualified Data.Text as T

#if MIN_VERSION_aeson(2,0,0)
type Key = Key.Key
#else
type Key = T.Text
#endif

toKey :: T.Text -> Key
#if MIN_VERSION_aeson(2,0,0)
toKey = Key.fromText
#else
toKey = id
#endif
