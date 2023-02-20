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
import qualified Data.Aeson.Optics as Optics
import Data.Text (Text)
import Optics.Operators ((^?))
import Optics.Optic ((%))
import TestContainers.Docker.Internal (InspectOutput)

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
  case inspectOutput ^? Optics.key "State" of
    Just state -> State state
    Nothing -> State "dummy"

-- | Returns the 'Status' of container.
--
-- @since 0.5.0.0
stateStatus :: State -> Status
stateStatus (State value) =
  case value
    ^? Optics.key "Status"
    % Optics._String of
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
  case value
    ^? Optics.key "OOMKilled"
    % Optics._Bool of
    Just True -> True
    _ -> False

-- |
--
-- @since 0.5.0.0
statePid :: State -> Maybe Int
statePid (State value) =
  case value
    ^? Optics.key "Pid"
    % Optics._Integer of
    Just pid -> Just (fromIntegral pid)
    _ -> Nothing

-- |
--
-- @since 0.5.0.0
stateExitCode :: State -> Maybe Int
stateExitCode (State value) =
  case value
    ^? Optics.key "ExitCode"
    % Optics._Integer of
    Just exitCode -> Just (fromIntegral exitCode)
    _ -> Nothing

-- |
--
-- @since 0.5.0.0
stateError :: State -> Maybe Text
stateError (State value) =
  case value
    ^? Optics.key "Error"
    % Optics._String of
    Just err -> Just err
    _ -> Nothing

-- |
--
-- @since 0.5.0.0
stateStartedAt :: State -> Maybe Text
stateStartedAt (State value) =
  case value
    ^? Optics.key "StartedAt"
    % Optics._String of
    Just err -> Just err
    _ -> Nothing

-- |
--
-- @since 0.5.0.0
stateFinishedAt :: State -> Maybe Text
stateFinishedAt (State value) =
  case value
    ^? Optics.key "FinishedAt"
    % Optics._String of
    Just err -> Just err
    _ -> Nothing
