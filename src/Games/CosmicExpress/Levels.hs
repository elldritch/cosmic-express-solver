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

import Data.ByteString qualified as BS
import Math.Geometry.Grid (Index, size)
import Math.Geometry.Grid.Square (RectSquareGrid)
import Rainbow (Chunk, bold, chunksToByteStrings, color256, fore, inverse, toByteStringsColors256)

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
  Critter c done -> renderChunk $ colorize c $ if done then "c" else bold "C"
  House c done -> renderChunk $ colorize c $ if done then "h" else bold "H"
 where
  -- For a lookup table of colors, see https://en.wikipedia.org/wiki/ANSI_escape_code#8-bit.
  colorize :: Color -> Chunk -> Chunk
  colorize Purple = inverse . fore (color256 91)
  colorize Orange = inverse . fore (color256 208)

  renderChunk :: Chunk -> String
  renderChunk c = decodeUtf8 $ BS.concat $ chunksToByteStrings toByteStringsColors256 [c]

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
