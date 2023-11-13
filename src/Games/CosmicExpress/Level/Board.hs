module Games.CosmicExpress.Level.Board (
  Board,
  Position,
  neighbor,
  Cardinal (..),
  cardinals,
  Bearing (..),
  turn,
  neighbors,
) where

import Relude

import Data.Aeson (FromJSON, ToJSON)
import Relude.Extra.Map (lookup)

-- A rectangular square board of tiles. For geometry terminology definitions,
-- see
-- https://hackage.haskell.org/package/grid-7.8.15/docs/Math-Geometry-Grid.html.
--
-- This is an implementation of @RectSquareGrid@ from the @grid@ package, but
-- with added flexibility for handling non-rectangular grids and better
-- direction handling.
type Board t = Map Position t

type Position = (Int, Int)

-- Cardinal directions.
data Cardinal = North | South | East | West
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

cardinals :: [Cardinal]
cardinals = [North, South, East, West]

-- Relative directions.
data Bearing = Forward | Backward | Rightward | Leftward
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

turn :: Cardinal -> Bearing -> Cardinal
turn North Forward = North
turn North Backward = South
turn North Rightward = East
turn North Leftward = West
turn South Forward = South
turn South Backward = North
turn South Rightward = West
turn South Leftward = East
turn East Forward = East
turn East Backward = West
turn East Rightward = South
turn East Leftward = North
turn West Forward = West
turn West Backward = East
turn West Rightward = North
turn West Leftward = South

-- The neighbor of a position in a cardinal direction.
neighbor :: Board t -> Position -> Cardinal -> Maybe (Position, t)
neighbor board (x, y) cardinal = (position',) <$> position' `lookup` board
 where
  (dx, dy) = case cardinal of
    North -> (0, 1)
    South -> (0, -1)
    East -> (1, 0)
    West -> (-1, 0)

  position' = (x + dx, y + dy)

-- All neighbors of a position.
neighbors :: Board t -> Position -> [(Position, t)]
neighbors board position = mapMaybe (neighbor board position) cardinals
