module Games.CosmicExpress.Levels (
  Tile (..),
  Color (..),
  Piece (..),
  Position,
  Level (..),
  renderLevel,
) where

import Relude
import Relude.Extra.Map (keys, lookup)

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.ByteString qualified as BS
import Rainbow (Chunk, bold, chunksToByteStrings, color256, fore, inverse, toByteStringsColors256)
import Relude.Extra.Foldable1 (Foldable1 (minimum1), maximum1)

-- The color of a Critter or House.
data Color = Purple | Orange
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- A single track piece. A track piece is defined by the edges of the tile that
-- the piece connects.
data Piece
  = NS
  | EW
  | NE
  | SE
  | NW
  | SW
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- A tile on the game board.
data Tile
  = Track Piece
  | Critter {color :: Color, completed :: Bool}
  | House {color :: Color, completed :: Bool}
  | Empty
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- Render a Tile in a human-friendly format for display inside a Level.
--
-- This function returns a string so it can use ANSI codes for color and
-- styling. Make sure that this string is always one-width, otherwise the grid
-- layout will break.
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
  Empty -> " "
 where
  -- For a lookup table of colors, see https://en.wikipedia.org/wiki/ANSI_escape_code#8-bit.
  colorize :: Color -> Chunk -> Chunk
  colorize Purple = inverse . fore (color256 91)
  colorize Orange = inverse . fore (color256 208)

  renderChunk :: Chunk -> String
  renderChunk c = decodeUtf8 $ BS.concat $ chunksToByteStrings toByteStringsColors256 [c]

type Position = (Int, Int)

data Level = Level
  { -- All board tiles are stored on the map.
    --
    -- (0, 0) is the bottom left corner. Row number goes up as tile positions go
    -- upwards. Column number goes up as tile positions go rightwards.
    tiles :: Map Position Tile
  , start :: Position
  , finish :: Position
  }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- Render a Level in a human-friendly format.
renderLevel :: Level -> String
renderLevel Level{tiles, start, finish} = rendered
 where
  positions = fromMaybe (error "renderLevel: impossible: level has no tiles") $ nonEmpty $ keys tiles
  columns = fst <$> positions
  rows = snd <$> positions
  minColumn = minimum1 columns
  maxColumn = maximum1 columns
  minRow = minimum1 rows
  maxRow = maximum1 rows
{- FOURMOLU_DISABLE -}
  rendered =
    ['┌'] ++ [ '─' | _ <- [minColumn..maxColumn] ] ++ ['┐'] ++ ['\n']
    ++ concat [
        ['│']
        ++
          concat [
            if (c, r) == start then "S" else
            if (c, r) == finish then "F" else
            maybe "X" renderTile (lookup (c, r) tiles)
          | c <- [minColumn..maxColumn] ]
        ++
        ['│'] ++ ['\n']
      | r <- reverse [minRow..maxRow] ]
    ++ ['└'] ++ [ '─' | _ <- [minColumn..maxColumn] ] ++ ['┘']
{- FOURMOLU_ENABLE -}
