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
-- @since x.x.x
data StateInvalidException = StateInvalidException
  deriving stock (Eq, Show)

instance Exception StateInvalidException

-- | Status of a Docker container.
--
-- @since x.x.x
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
-- @since x.x.x
newtype State = State Value

-- | Extract the 'State' of a Docker container from an 'InspectOutput'.
--
-- @since x.x.x
containerState :: InspectOutput -> State
containerState inspectOutput =
  case inspectOutput ^? Optics.key "State" of
    Just state -> State state
    Nothing -> State "dummy"

-- | Returns the 'Status' of container.
--
-- @since x.x.x
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
-- @since x.x.x
stateOOMKilled :: State -> Bool
stateOOMKilled (State value) =
  case value
    ^? Optics.key "OOMKilled"
    % Optics._Bool of
    Just True -> True
    _ -> False

-- |
--
-- @since x.x.x
statePid :: State -> Maybe Int
statePid (State value) =
  case value
    ^? Optics.key "Pid"
    % Optics._Integer of
    Just pid -> Just (fromIntegral pid)
    _ -> Nothing

-- |
--
-- @since x.x.x
stateExitCode :: State -> Maybe Int
stateExitCode (State value) =
  case value
    ^? Optics.key "ExitCode"
    % Optics._Integer of
    Just exitCode -> Just (fromIntegral exitCode)
    _ -> Nothing

-- |
--
-- @since x.x.x
stateError :: State -> Maybe Text
stateError (State value) =
  case value
    ^? Optics.key "Error"
    % Optics._String of
    Just err -> Just err
    _ -> Nothing

-- |
--
-- @since x.x.x
stateStartedAt :: State -> Maybe Text
stateStartedAt (State value) =
  case value
    ^? Optics.key "StartedAt"
    % Optics._String of
    Just err -> Just err
    _ -> Nothing

-- |
--
-- @since x.x.x
stateFinishedAt :: State -> Maybe Text
stateFinishedAt (State value) =
  case value
    ^? Optics.key "FinishedAt"
    % Optics._String of
    Just err -> Just err
    _ -> Nothing
