module Games.CosmicExpress.Solve (solve) where

import Relude
import Relude.Extra.Foldable1 (maximum1)
import Relude.Extra.Map (insert, toPairs)

import Algorithm.Search (aStarAssoc)
import Data.Aeson (encode)

import Games.CosmicExpress.Debug (debugLn, debugWith)
import Games.CosmicExpress.Level (
  Color (..),
  Level (..),
  Piece (..),
  Position,
  Tile (..),
  isCritter,
  isEmpty,
  isHouse',
  renderLevel',
 )
import Games.CosmicExpress.Level.Board (
  Bearing (..),
  Cardinal (..),
  cardinals,
  distance,
  neighbor,
  neighbors,
  turn,
 )
import Games.CosmicExpress.Solve.Solvability (solvable, solved)
import Games.CosmicExpress.Solve.Step (Step (..))

-- Render a step for debugging.
_renderStep :: String -> Step -> String
_renderStep prefix s@Step{level, tip, train, previousPosition} = rendered
 where
  renderer position
    | position == tip = Just "T"
    | position == previousPosition = Just "X"
    | otherwise = Nothing

{- FOURMOLU_DISABLE -}
  rendered =
    prefix ++ ": " ++ "\n" ++
    renderLevel' level renderer ++ "\n" ++
    "Train: " ++ show train ++ "\n" ++
    "Tip: " ++ show tip ++ "\n" ++
    "Heuristic: " ++ show (heuristic s) ++ "\n" ++
    "Solvable: " ++ show (solvable s) ++ "\n" ++
    "Show: " ++ show s ++ "\n" ++
    "JSON: " ++ decodeUtf8 (encode s)
{- FOURMOLU_ENABLE -}

-- Solve a level.
--
-- We solve levels using a graph search through possible game states.
solve :: Level -> Level
solve level = case solution of
  Nothing -> error "Impossible: no solution found"
  Just s -> s
 where
  solution :: Maybe Level
  solution = do
    -- Calculate the path of game states from the initial game state to a
    -- completed game state.
    (_, steps) <- aStarAssoc (fmap (,1) . next) heuristic found $ initial level
    Step{tip, level = l@Level{tiles}, previousExitBearing} <- last <$> nonEmpty steps
    -- Add the final track piece, which connects the final tip tile to the exit
    -- tunnel in the East.
    pure l{tiles = insert tip (Track $ connect previousExitBearing East) tiles}

-- We begin facing east, with the tip at the start tile, with no critter in
-- the train.
initial :: Level -> Step
initial level =
  Step
    { level = level
    , tip = start
    , previousPosition
    , previousExitBearing = East
    , train = Nothing
    }
 where
  Level{start} = level
  -- The previous position (where the train car starts) is always one tile
  -- to the West of the start tile, since the start tunnel always faces
  -- East.
  previousPosition = let (x, y) = start in (x - 1, y)

-- We have reached our destination the level is solved
--
-- We can now trivially complete the track by connecting the finish tile.
found :: Step -> Bool
found step = debugFound $ solved step
 where
  debugFound = debugLn $ _renderStep "Testing" step

-- For A* search, we need a heuristic function that never overestimates the
-- distance to the goal. We use the maximum Manhattan distance from the tip to
-- any unfinished objective, or to the finish tile.
heuristic :: Step -> Int
heuristic Step{level = Level{tiles, finish}, tip} =
  maximum1 $ distance tip <$> finish :| fmap fst (filter (isUnfinishedObjective . snd) $ toPairs tiles)
 where
  isUnfinishedObjective (House _ False) = True
  isUnfinishedObjective (Critter _ False) = True
  isUnfinishedObjective _ = False

-- To calculate the next steps, we need to do three things:
--
-- 1. Select a next tile to try, and place a track piece to that new tip.
-- 2. Simulate the effects of the train engine reaching that new tip tile. This
--    means simulating the arrival of the train car to the previous tile (i.e.
--    the current tip).
-- 3. Determine whether the proposed track in the next step is still solvable.
--    There are several heuristics we can use to abort early if we've reached a
--    state where we obviously can no longer complete the level. This
--    determination must occur _after_ simulating the current arrival, since the
--    arrival of the train car (and therefore delivery/pickup of some critters)
--    may change the solvability of a particular track position.
--
next :: Step -> [Step]
next step@Step{level = level@Level{tiles}, tip, previousExitBearing} = debugNext $ do
  -- Abort if the current track is no longer solvable.
  guard $ solvable step

  -- Select a next tile to try.
  direction <- cardinals
  (position, tile) <- maybeToList $ neighbor tiles tip direction
  -- Make sure the selected tile is empty.
  guard $ isEmpty tile
  -- Place a track piece to the new selected tip, and update the tip and
  -- previous positions.
  let step' =
        step
          { level = level{tiles = insert tip (Track $ connect previousExitBearing direction) tiles}
          , tip = position
          , previousPosition = tip
          , previousExitBearing = direction
          }

  -- Simulate the arrival of the train car to the new tip.
  let step'' = simulateTrain step'

  pure step''
 where
  debugNext :: [Step] -> [Step]
  debugNext = debugWith message
   where
{- FOURMOLU_DISABLE -}
    message nexts =
      _renderStep "Current" step ++ "\n" ++
        "\n" ++
        "Nexts: " ++ show (length nexts) ++ "\n" ++
        concat [_renderStep ("Next " ++ show i) n ++ "\n" | (i, n) <- zip ([1 ..] :: [Int]) nexts]
{- FOURMOLU_ENABLE -}

-- @connect a b@ computes the track piece that connects an existing tile facing
-- @a@ to a new tile in the direction of @b@.
--
-- This function is partial. Passing an invalid direction (i.e. one that implies
-- a track piece that doubles back on itself) will result in an error.
connect :: Cardinal -> Cardinal -> Piece
connect start end = case start of
  North -> case end of
    North -> NS
    East -> SE
    West -> SW
    South -> doublesBack
  East -> case end of
    North -> NW
    East -> EW
    West -> doublesBack
    South -> SW
  South -> case end of
    North -> doublesBack
    East -> NE
    West -> NW
    South -> NS
  West -> case end of
    North -> NE
    East -> doublesBack
    West -> EW
    South -> SE
 where
  doublesBack = error "Impossible: track piece doubles back on itself"

-- Simulate the arrival of the train engine to the tip tile (and therefore the
-- train car to the previous tile).
simulateTrain :: Step -> Step
simulateTrain step = step''
 where
  -- First, we deliver any critters currently on the train if possible.
  step' = deliverCritters step
  -- Second, we pick up any critters adjacent to the train if possible.
  step'' = pickupCritters step'

-- Deliver a critter on the train car. The train car is located on the
-- previously placed tile.
deliverCritters :: Step -> Step
deliverCritters step@Step{level = level@Level{tiles}, train, previousPosition, previousExitBearing} =
  fromMaybe step $ do
    -- If the train has no critter, exit.
    color <- train
    -- If the train has a critter, look for a house to deliver it to.
    --
    -- The house must match the color of the critter. If there are two such
    -- houses, we pick houses in priority order of their relative bearing:
    --
    -- 1. Rightward
    -- 2. Leftward
    -- 3. Backward
    --
    -- Note that there cannot be more than two adjacent houses, since two of the
    -- train car's tile's edges are adjacent to tracks. Note that there also can
    -- never be an adjacent house in the Forward bearing, since the tile in that
    -- direction will be occupied by the exit track piece (i.e. the current tip
    -- tile's eventual track piece).
    --
    -- If there is no matching house, exit.
    (position, _) <- firstMatchingHouse color
    pure
      step
        { level = level{tiles = insert position (House color True) tiles}
        , train = Nothing
        }
 where
  neighborsInPriorityOrder :: [(Position, Tile)]
  neighborsInPriorityOrder =
    mapMaybe
      (neighbor tiles previousPosition . turn previousExitBearing)
      [Rightward, Leftward, Backward]

  firstMatchingHouse :: Color -> Maybe (Position, Tile)
  firstMatchingHouse c = find (isHouse' c . snd) neighborsInPriorityOrder

-- Pick up a critter adjacent to the train car.
pickupCritters :: Step -> Step
pickupCritters step@Step{level = level@Level{tiles}, train, previousPosition} = case train of
  -- If the train is already full, exit.
  Just _ -> step
  Nothing -> fromMaybe step $ do
    -- Otherwise, grab any single adjacent critter.
    --
    -- If there are two adjacent critters, they will bounce off of each other,
    -- so grab neither.
    --
    -- Note that there cannot be more than two adjacent critters, since two of the
    -- train car's tile's edges are adjacent to tracks.
    --
    -- If there are no adjacent critters, exit.
    ns <- nonEmpty $ filter (isCritter . snd) $ neighbors tiles previousPosition
    case ns of
      (p, Critter c False) :| [] ->
        pure
          step
            { train = Just c
            , level = level{tiles = insert p (Critter c True) tiles}
            }
      _ -> mzero
