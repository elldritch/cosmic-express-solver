module Games.CosmicExpress.Solve.Solvability (solved, solvable) where

import Relude
import Relude.Extra.Map (elems, toPairs)

import Algorithm.Search (aStarAssoc)

import Games.CosmicExpress.Debug (debugPrefix)
import Games.CosmicExpress.Level (Level (..), Position, Tile (..), isHouse', isIncomplete)
import Games.CosmicExpress.Level.Board (Board, distance, neighbors)
import Games.CosmicExpress.Solve.Step (Step (..))

-- We have reached our destination if all of the following are true:
--
-- 1. We have reached the finish tile.
-- 2. Every critter has been delivered to its house.
--
-- We can now trivially complete the track by connecting the finish tile.
solved :: Step -> Bool
solved Step{level = Level{tiles, finish}, tip} =
  tip == finish && not (any isIncomplete (elems tiles))

-- Return False if the current track is obviously unsolvable.
solvable :: Step -> Bool
solvable Step{level = Level{tiles, finish}, tip, train} = and criteria
 where
  criteria :: [Bool]
  criteria =
    debugPrefix
      "criteria"
      [ canDropOff
      , notIncompleteAtFinish
      , finishAccessible
      , incompleteAccessible
      , crittersHaveHouses
      ]

  -- Remaining incomplete objectives.
  incomplete :: [(Position, Tile)]
  incomplete = filter (isIncomplete . snd) $ toPairs tiles

  -- If I have a critter, I need to be able to reach a house for that critter.
  canDropOff = case train of
    Nothing -> True
    Just color ->
      any
        (canReach tiles tip . fst)
        (filter (isHouse' color . snd) incomplete)

  -- If we're at the finish tile and there are incomplete objectives, then the
  -- current track is unsolvable since we cannot possibly double back to reach
  -- objectives after already arriving at the finish tile.
  notIncompleteAtFinish = (tip /= finish) || null incomplete

  -- End tile must be accessible.
  finishAccessible = canReach tiles tip finish

  -- All unfinished objectives must be accessible.
  incompleteAccessible = all (canReach tiles tip . fst) incomplete

  -- All remaining critters must need a path to a remaining house for them.
  crittersHaveHouses = all critterHasHouse incomplete
   where
    critterHasHouse (position, Critter color False) =
      any (canReach tiles position . fst) (filter (isHouse' color . snd) incomplete)
    critterHasHouse _ = True

-- Returns True if there is a path of `Empty` tiles from start to end, and False
-- otherwise.
canReach :: Board Tile -> Position -> Position -> Bool
canReach tiles start end = isJust $ aStarAssoc next heuristic found initial
 where
  next :: Position -> [(Position, Int)]
  next pos =
    (fmap . second) (const 1)
      $ filter (\(p, tile) -> p == end || isEmpty tile)
      $ neighbors tiles pos
   where
    isEmpty Empty = True
    isEmpty _ = False

  heuristic :: Position -> Int
  heuristic = distance end

  found :: Position -> Bool
  found = (== end)

  initial :: Position
  initial = start
