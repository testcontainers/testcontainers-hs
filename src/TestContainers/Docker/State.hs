{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module TestContainers.Docker.State
  ( State,
    containerState,

    -- * Container status
    Status (..),
    stateStatus,
    stateOOMKilled,
    statePid,
    stateExitCode,
    stateError,
    stateStartedAt,
    stateFinishedAt,
  )
where

import Control.Exception (Exception, throw)
import Data.Aeson (Value)
import Data.Text (Text)
import TestContainers.Docker.Internal (InspectOutput)
import TestContainers.Docker.JSON (asBool, asInteger, asText, lookupKey)

-- | An exception thrown in case the State object is invalid and couldn't be parsed.
--
-- @since 0.5.0.0
data StateInvalidException = StateInvalidException
  deriving stock (Eq, Show)

instance Exception StateInvalidException

-- | Status of a Docker container.
--
-- @since 0.5.0.0
data Status
  = Created
  | Running
  | Paused
  | Restarting
  | Removing
  | Exited
  | Dead
  | Other Text
  deriving (Eq, Show)

-- | State of a Docker container.
--
-- @since 0.5.0.0
newtype State = State Value

-- | Extract the 'State' of a Docker container from an 'InspectOutput'.
--
-- @since 0.5.0.0
containerState :: InspectOutput -> State
containerState inspectOutput =
  case lookupKey "State" inspectOutput of
    Just state -> State state
    Nothing -> State "dummy"

-- | Returns the 'Status' of container.
--
-- @since 0.5.0.0
stateStatus :: State -> Status
stateStatus (State value) =
  case lookupKey "Status" value >>= asText of
    Just "created" -> Created
    Just "running" -> Running
    Just "paused" -> Paused
    Just "restarting" -> Restarting
    Just "removing" -> Removing
    Just "exited" -> Exited
    Just "dead" -> Dead
    Just other -> Other other
    Nothing -> throw StateInvalidException

-- | Whether a container was killed by the OOM killer.
--
-- @since 0.5.0.0
stateOOMKilled :: State -> Bool
stateOOMKilled (State value) =
  case lookupKey "OOMKilled" value >>= asBool of
    Just True -> True
    _ -> False

-- |
--
-- @since 0.5.0.0
statePid :: State -> Maybe Int
statePid (State value) =
  case lookupKey "Pid" value >>= asInteger of
    Just pid -> Just (fromIntegral pid)
    _ -> Nothing

-- |
--
-- @since 0.5.0.0
stateExitCode :: State -> Maybe Int
stateExitCode (State value) =
  case lookupKey "ExitCode" value >>= asInteger of
    Just exitCode -> Just (fromIntegral exitCode)
    _ -> Nothing

-- |
--
-- @since 0.5.0.0
stateError :: State -> Maybe Text
stateError (State value) =
  case lookupKey "Error" value >>= asText of
    Just err -> Just err
    _ -> Nothing

-- |
--
-- @since 0.5.0.0
stateStartedAt :: State -> Maybe Text
stateStartedAt (State value) =
  case lookupKey "StartedAt" value >>= asText of
    Just err -> Just err
    _ -> Nothing

-- |
--
-- @since 0.5.0.0
stateFinishedAt :: State -> Maybe Text
stateFinishedAt (State value) =
  case lookupKey "FinishedAt" value >>= asText of
    Just err -> Just err
    _ -> Nothing
