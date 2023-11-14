module Games.CosmicExpress.Solve.Step (Step (..)) where

import Relude

import Data.Aeson (ToJSON)

import Games.CosmicExpress.Level (Color, Level, Position)
import Games.CosmicExpress.Level.Board (Cardinal)

-- A Step represents a snapshot of game with a partial solution. It captures a
-- set of tracks laid down and tracks the state of the train as it follows those
-- tracks.
data Step = Step
  { -- The level we're currently solving.
    level :: Level
  , -- The "tip" is the next tile that needs to have a track piece placed in it.
    -- This tile is currently empty in this step.
    --
    -- When simulating the train, we model this as the position that the train
    -- engine is currently in. This lines up well with our chosen end condition
    -- in `found` of the tip ending in the finish tile, which is where the train
    -- engine would be.
    tip :: Position
  , -- The previous tile that was placed. This tile has a track piece in it.
    --
    -- When simulating the train, we model this as the position that the train
    -- car is currently in.
    previousPosition :: Position
  , -- The bearing of the train car as it exits the previous tile that was
    -- placed.
    previousExitBearing :: Cardinal
  , -- The critter currently in the train.
    train :: Maybe Color
  }
  deriving (Eq, Ord, Show, Generic, ToJSON)
