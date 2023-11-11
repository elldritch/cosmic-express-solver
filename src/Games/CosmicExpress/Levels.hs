module Games.CosmicExpress.Levels (
  Tile (..),
  Color (..),
  Orientation (..),
  grid,
  rows,
  columns,
  Position,
  Level (..),
  renderLevel,
) where

import Relude
import Relude.Extra.Map (lookup)

import Math.Geometry.Grid (Index)
import Math.Geometry.Grid.Square (RectSquareGrid, rectSquareGrid)
import System.Console.Pretty (Style (..), color, style)
import System.Console.Pretty qualified as Console (Color (..))

data Color = Purple
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
  (Track t) -> case t of
    NS -> "║"
    EW -> "═"
    NE -> "╚"
    SE -> "╔"
    NW -> "╝"
    SW -> "╗"
  (Critter c done) -> renderColored c $ if done then "c" else style Bold "C"
  (House c done) -> renderColored c $ if done then "h" else style Bold "H"
 where
  renderColored :: Color -> String -> String
  renderColored Purple l = style Reverse $ color Console.Magenta l

rows :: Int
rows = 7

columns :: Int
columns = 8

-- (0, 0) is the bottom left corner. Row number goes up as tile positions go
-- upwards. Column number goes up as tile positions go rightwards.
--
-- See visualization at https://github.com/mhwombat/grid/wiki/Square-tiles. We
-- need to match this visualization so that Directions from the grid library
-- work as expected.
grid :: RectSquareGrid
grid = rectSquareGrid rows columns

type Position = Index RectSquareGrid

data Level = Level
  { -- Only non-empty tiles are stored in the map.
    tiles :: Map Position Tile
  , start :: Int
  , finish :: Int
  }
  deriving (Eq, Ord, Show)

{- FOURMOLU_DISABLE -}
renderLevel :: Level -> String
renderLevel Level{tiles, start, finish} =
  ['┌'] ++ [ '─' | _ <- [0..columns-1] ] ++ ['┐'] ++ ['\n']
  ++ concat [
    [if r == start then 'S' else '│']
    ++
    concat [ maybe " " renderTile (lookup (c, r) tiles) | c <- [0..columns-1] ]
    ++
    [if r == finish then 'F' else '│']
    ++
    ['\n']
  | r <- reverse [0..rows-1] ]
  ++ ['└'] ++ [ '─' | _ <- [0..columns-1] ] ++ ['┘']
{- FOURMOLU_ENABLE -}
