module Games.CosmicExpress.Levels (
  Tile (..),
  Color (..),
  Orientation (..),
  Position,
  Level (..),
  Grid (..),
  renderLevel,
) where

import Relude
import Relude.Extra.Map (lookup)

import Math.Geometry.Grid (Index, size)
import Math.Geometry.Grid.Square (RectSquareGrid)
import System.Console.Pretty (Style (..), color, style)
import System.Console.Pretty qualified as Console (Color (..))

data Color = Purple | Orange
  deriving (Eq, Ord, Show)

data Orientation
  = NS
  | EW
  | NE
  | SE
  | NW
  | SW
  deriving (Eq, Ord, Show)

data Tile
  = Track Orientation
  | Critter {color :: Color, completed :: Bool}
  | House {color :: Color, completed :: Bool}
  deriving (Eq, Ord, Show)

-- Returns a string for the sake of colorizing ANSI codes. Make sure that this
-- string is always one-width, otherwise the grid layout will break.
renderTile :: Tile -> String
renderTile = \case
  Track t -> case t of
    NS -> "║"
    EW -> "═"
    NE -> "╚"
    SE -> "╔"
    NW -> "╝"
    SW -> "╗"
  Critter c done -> renderColored c $ if done then "c" else style Bold "C"
  House c done -> renderColored c $ if done then "h" else style Bold "H"
 where
  renderColored :: Color -> String -> String
  renderColored Purple l = style Reverse $ color Console.Magenta l
  renderColored Orange l = style Reverse $ color Console.Yellow l

-- This is a newtype wrapper over RectSquareGrid so that we can define an Ord.
newtype Grid = Grid RectSquareGrid
  deriving (Eq, Show)

instance Ord Grid where
  compare :: Grid -> Grid -> Ordering
  compare _ _ = EQ

type Position = Index RectSquareGrid

data Level = Level
  { -- Only non-empty tiles are stored in the map.
    tiles :: Map Position Tile
  , start :: Position
  , finish :: Position
  , -- (0, 0) is the bottom left corner. Row number goes up as tile positions go
    -- upwards. Column number goes up as tile positions go rightwards.
    --
    -- See visualization at https://github.com/mhwombat/grid/wiki/Square-tiles.
    -- We need to match this visualization so that Directions from the grid
    -- library work as expected.
    grid :: Grid
  }
  deriving (Eq, Ord, Show)

{- FOURMOLU_DISABLE -}
renderLevel :: Level -> String
renderLevel Level{tiles, start, finish, grid = Grid grid} =
  ['┌'] ++ [ '─' | _ <- [0..columns-1] ] ++ ['┐'] ++ ['\n']
  ++ concat [
    ['│']
    ++
    concat [
      if (c, r) == start then "S" else
      if (c, r) == finish then "F" else
      maybe " " renderTile (lookup (c, r) tiles)
    | c <- [0..columns-1] ]
    ++
    ['│']
    ++
    ['\n']
  | r <- reverse [0..rows-1] ]
  ++ ['└'] ++ [ '─' | _ <- [0..columns-1] ] ++ ['┘']
 where
  (rows, columns) = size grid
{- FOURMOLU_ENABLE -}
