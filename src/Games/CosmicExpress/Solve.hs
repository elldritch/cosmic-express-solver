module Games.CosmicExpress.Solve (solve) where

import Relude

import Algorithm.Search (aStarAssoc, bfs)
import Games.CosmicExpress.Levels (Color (..), Level (..), Orientation (..), Position, Tile (..), columns, grid, renderLevel)
import Math.Geometry.Grid (distance, neighbour, neighbours)
import Math.Geometry.Grid.SquareInternal (SquareDirection (..))
import Relude.Extra.Foldable1 (Foldable1 (minimum1))
import Relude.Extra.Map (elems, insert, lookup, member, toPairs)

-- This is a newtype wrapper over SquareDirection so that we can define an Ord.
newtype Direction = Direction SquareDirection
  deriving (Eq, Show)

instance Ord Direction where
  compare :: Direction -> Direction -> Ordering
  compare (Direction a) (Direction b) = compare (toInt a) (toInt b)
   where
    toInt :: SquareDirection -> Int
    toInt North = 1
    toInt East = 2
    toInt South = 3
    toInt West = 4

directions :: [SquareDirection]
directions = [North, East, South, West]

data Step = Step
  { level :: Level
  , -- The "tip" is the next tile that needs to have a track piece placed in it.
    -- This tile is currently empty in this step.
    tip :: Position
  , -- The direction we're facing after placing the previous track piece. If we
    -- last placed an EW track piece moving rightwards, we came from the West and
    -- would be "facing" East.
    facing :: Direction
  , -- The critter currently in the train.
    train :: Maybe Color
  }
  deriving (Eq, Ord, Show)

debug :: (Show a) => String -> a -> a
debug prefix a = trace (prefix <> ": " <> show a) a

debugLen :: String -> [a] -> [a]
debugLen prefix a = trace (prefix <> ": " <> show (length a)) a

debugStep :: String -> Step -> Step
debugStep prefix s = debugStep' prefix s s

debugStep' :: String -> Step -> a -> a
debugStep' prefix s a = trace (debugStepMsg prefix s) a

{- FOURMOLU_DISABLE -}
debugStepMsg :: String -> Step -> String
debugStepMsg prefix s@Step{level, tip, facing, train} =
  prefix ++ ": " ++ show s ++ "\n" ++
  renderLevel level ++ "\n" ++
  "Train: " ++ show train ++ "\n" ++
  "Tip: " ++ show tip ++ "\n" ++
  "Facing: " ++ show facing ++ "\n" ++
  "Heuristic: " ++ show (heuristic s) ++ "\n"
{- FOURMOLU_ENABLE -}

solve :: Level -> Level
solve startLevel@Level{start, finish} = case solution of
  Nothing -> error "Impossible: no solution found"
  Just l -> l
 where
  -- We begin facing east, with the tip in front of the start tile, with no
  -- critter in the train.
  initial :: Step
  initial = debugStep "Start" $ Step{level = startLevel, tip = beginning, facing = Direction East, train = Nothing}

  -- We begin in front of the start tile.
  beginning :: Position
  beginning = (0, start)

  -- We end in front of the finish tile.
  end :: Position
  end = (columns - 1, finish)

  -- We have reached our destination if all of the following are true:
  --
  -- 1. We have reached the end tile.
  -- 2. Every critter has been delivered to its house.
  --
  -- We can now complete the track by connecting the end tile to the finish
  -- tile.
  found :: Step -> Bool
  found s@Step{level = Level{tiles}, tip} =
    debugStep' "Testing" s $ tip == end && all delivered (elems tiles)

  delivered :: Tile -> Bool
  delivered (Critter _ False) = False
  delivered (House _ False) = False
  delivered _ = True

  -- To find the next Step states, we need to do two things:
  --
  -- 1. Fill the current (empty) tip tile, which leads us to our next tip tile.
  -- 2. Calculate any adjacency effects from critters, houses, etc.
  --
  -- To fill the current tip tile, we need to choose a target next tip tile, and
  -- then fill the current tile with the appropriately oriented track piece. To
  -- do this, we find all neighbors and then filter out neighboring tiles that
  -- are occupied.
  --
  -- To calculate adjacency effects, we look at the current tip tile's
  -- neighbors:
  --
  -- 1. If any neighbor is a house and the train contains a critter of the
  --    correct color, unload the critter from the train and mark the house as
  --    completed.
  -- 2. If any neighbor is a critter and the train is empty, load the critter
  --    into the train and mark the critter as completed.
  --
  -- Note that A* also requires a cost for every step. The cost is always 1.
  next :: Step -> [Step]
  next
    currentStep@Step
      { level = currentLevel@Level{tiles = currentTiles}
      , tip = currentTip
      , facing = (Direction facing)
      } = debugNexts nextSteps
     where
      debugNexts :: [Step] -> [Step]
      debugNexts nexts = trace msg nexts
        where
          msg =
            debugStepMsg "Current" currentStep ++ "\n"
            ++ "Nexts: " ++ show (length nexts) ++ "\n"
            ++ concat [ debugStepMsg ("Next " <> show i) n | (i, n) <- zip ([1..] :: [Int]) nexts]

      trackPiece :: SquareDirection -> Orientation
      trackPiece nextDirection = case facing of
        North -> case nextDirection of
          North -> NS
          East -> SE
          West -> SW
          South -> doublesBack
        East -> case nextDirection of
          North -> NW
          East -> EW
          West -> doublesBack
          South -> SW
        South -> case nextDirection of
          North -> doublesBack
          East -> NE
          West -> NW
          South -> NS
        West -> case nextDirection of
          North -> NE
          East -> doublesBack
          West -> EW
          South -> SE
       where
        doublesBack = error "Impossible: track piece doubles back on itself"

      nextPositions :: [Step]
      nextPositions = do
        -- Special case: if the current tip is the end tile, but objectives are
        -- still incomplete, then this track is obviously impossible.
        guard $ not (currentTip == end && not (all delivered (elems currentTiles)))
        -- Choose a direction to go in.
        direction <- directions
        -- Make sure the next position is still within the grid.
        position <- maybeToList $ neighbour grid currentTip direction
        -- Make sure the next position is unoccupied.
        guard $ not $ member position currentTiles

        pure
          currentStep
            { level = currentLevel{tiles = insert currentTip (Track $ trackPiece direction) currentTiles}
            , tip = position
            , facing = Direction direction
            }

      neighborCritters :: [(Position, Color)]
      neighborCritters = mapMaybe critterLookup (neighbours grid currentTip)
       where
        critterLookup :: Position -> Maybe (Position, Color)
        critterLookup p = case lookup p currentTiles of
          Just (Critter c False) -> Just (p, c)
          _ -> Nothing

      neighborHouses :: Color -> [Position]
      neighborHouses c = filter hasHouse (neighbours grid currentTip)
       where
        hasHouse :: Position -> Bool
        hasHouse p = case lookup p currentTiles of
          Just (House h False) -> h == c
          _ -> False

      nextSteps :: [Step]
      nextSteps = do
        -- Take a step into the next position.
        step@Step{level = level@Level{tiles}, train} <- debugLen "nextPositions" nextPositions

        -- Make available house deliveries.
        step'@Step{level = level'@Level{tiles = tiles'}, train = train'} <- case train of
          Nothing -> pure step
          Just c -> do
            let houses = neighborHouses c
            maybeHouse <- if null houses then [Nothing] else Just <$> houses
            case maybeHouse of
              Nothing -> pure step
              Just house ->
                pure
                  step
                    { level = level{tiles = insert house (House c True) tiles}
                    , train = Nothing
                    }

        -- Make available critter pickups.
        case train' of
          Just _ -> pure step'
          Nothing -> do
            maybeCritter <- if null neighborCritters then [Nothing] else Just <$> neighborCritters
            case maybeCritter of
              Nothing -> pure step'
              Just (p, c) ->
                pure
                  step'
                    { level = level'{tiles = insert p (Critter c True) tiles'}
                    , train = Just c
                    }

  path = bfs next found initial

  solution = do
    steps <- path
    steps' <- nonEmpty steps
    pure $ (.level) $ last steps'

-- A* uses the heuristic to preferentially try paths "best-first". To produce
-- an optimal solution, our heuristic must be admissible, which means it must
-- never overestimate the distance to the destination.
--
-- A naive heuristic is taking the distance to the end tile. This results in
-- A* trying really hard to reach the end tile without completing the pickups
-- and drop-offs it needs to do on the way.
--
-- We also cannot use the distance to the nearest "incomplete objective". This
-- heuristic is defined as:
--
-- 1. When the train is empty but there are remaining critters, the distance
--    to the nearest critter.
-- 2. When the train is loaded, the distance to nearest house that the loaded
--    critter can be dropped off at.
-- 3. When all objectives are fulfilled, the distance to the end tile.
--
-- This heuristic doesn't work either because it causes the heuristic to spike
-- after accomplishing certain objectives. For example, after reaching a
-- nearby critter, the heuristic spikes to the distance to the critter's
-- house. This encourages A* to find paths that go towards the critter but
-- never reach it, in order to keep the low heuristic value.
--
-- Instead, we sum all of these distances, because we need to avoid the
-- heuristic ever increasing upon completing an objective.
heuristic :: Step -> Int
heuristic Step{level = Level{tiles, finish}, tip} =
  fromMaybe 0 nearestRemainingCritter
    + fromMaybe 0 nearestRemainingHouse
    + distance grid tip end
 where
  hasCritter :: Tile -> Bool
  hasCritter (Critter _ False) = True
  hasCritter _ = False

  hasHouse :: Tile -> Bool
  hasHouse (House _ False) = True
  hasHouse _ = False

  nearestRemaining :: (Tile -> Bool) -> Maybe Int
  nearestRemaining f = fmap minimum1 $ nonEmpty $ fmap (distance grid tip . fst) $ filter (f . snd) $ toPairs tiles

  nearestRemainingCritter = nearestRemaining hasCritter
  nearestRemainingHouse = nearestRemaining hasHouse

  -- We end in front of the finish tile.
  end :: Position
  end = (columns - 1, finish)
