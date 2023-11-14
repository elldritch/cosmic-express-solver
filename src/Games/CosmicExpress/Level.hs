module Games.CosmicExpress.Level (
  Tile (..),
  isCritter,
  isHouse,
  isHouse',
  isEmpty,
  isIncomplete,
  Color (..),
  Piece (..),
  Position,
  Level (..),
  renderLevel,
  renderLevel',
) where

import Relude
import Relude.Extra.Foldable1 (Foldable1 (minimum1), maximum1)
import Relude.Extra.Map (keys, lookup)

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.ByteString qualified as BS
import Rainbow (Chunk, bold, chunksToByteStrings, color256, fore, inverse, toByteStringsColors256)

import Games.CosmicExpress.Level.Board (Board, Position)

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

-- Returns True if the Tile contains a Critter that has not yet been picked up.
isCritter :: Tile -> Bool
isCritter (Critter _ False) = True
isCritter _ = False

-- Returns True if the Tile contains a House that has not yet been filled.
isHouse :: Tile -> Bool
isHouse (House _ False) = True
isHouse _ = False

-- Like `isHouse`, but for Houses of a specific Color.
isHouse' :: Color -> Tile -> Bool
isHouse' c (House h False) = c == h
isHouse' _ _ = False

-- Returns True if the Tile is Empty.
isEmpty :: Tile -> Bool
isEmpty Empty = True
isEmpty _ = False

-- Returns True if the Tile contains an objective that is incomplete.
isIncomplete :: Tile -> Bool
isIncomplete (Critter _ False) = True
isIncomplete (House _ False) = True
isIncomplete _ = False

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

data Level = Level
  { -- All valid board tiles, including empty ones, are stored in the map.
    --
    -- (0, 0) is the bottom left corner. Row number goes up as tile positions go
    -- upwards. Column number goes up as tile positions go rightwards.
    tiles :: Board Tile
  , start :: Position
  , finish :: Position
  }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- Render a Level in a human-friendly format.
renderLevel :: Level -> String
renderLevel level = renderLevel' level $ const Nothing

-- Render a Level in a human-friendly format. Given a custom render function,
-- it can render certain tiles differently based on their position. This allows
-- consumers to mark tiles of interest.
renderLevel' :: Level -> (Position -> Maybe String) -> String
renderLevel' Level{tiles, start, finish} render = rendered
 where
  positions = fromMaybe (error "renderLevel: impossible: level has no tiles") $ nonEmpty $ keys tiles
  columns = fst <$> positions
  rows = snd <$> positions
  minColumn = minimum1 columns
  maxColumn = maximum1 columns
  minRow = minimum1 rows
  maxRow = maximum1 rows
  renderPosition (x, y) = case render (x, y) of
    Just s -> s
    Nothing | (x, y) == start -> "S"
    Nothing | (x, y) == finish -> "F"
    Nothing -> maybe "X" renderTile (lookup (x, y) tiles)
{- FOURMOLU_DISABLE -}
  rendered =
    -- Draw the top border.
    ['┌'] ++ ['─' | _ <- [minColumn..maxColumn]] ++ ['┐'] ++ ['\n']
    -- Draw the level's tiles.
    ++ concat [
        -- Draw the left border.
        ['│'] ++
        -- Draw the tile.
        concat [ renderPosition (c, r) | c <- [minColumn..maxColumn] ]
        -- Draw the right border.
        ++ ['│'] ++ ['\n']
    | r <- reverse [minRow..maxRow] ]
    -- Draw the bottom border.
    ++ ['└'] ++ ['─' | _ <- [minColumn..maxColumn]] ++ ['┘']
{- FOURMOLU_ENABLE -}
