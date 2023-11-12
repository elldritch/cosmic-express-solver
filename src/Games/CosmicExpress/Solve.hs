module Games.CosmicExpress.Solve (solve) where

import Relude
import Relude.Extra.Map (elems, insert, lookup, member)

import Algorithm.Search (bfs)
import Math.Geometry.Grid (neighbour, neighbours)
import Math.Geometry.Grid.SquareInternal (SquareDirection (..))

import Games.CosmicExpress.Levels (
  Color (..),
  Grid (..),
  Level (..),
  Orientation (..),
  Position,
  Tile (..),
  renderLevel,
 )

-- This is a newtype wrapper over SquareDirection so that we can define an Ord.
newtype Direction = Direction SquareDirection
  deriving (Eq, Show)

instance Ord Direction where
  compare :: Direction -> Direction -> Ordering
  compare _ _ = EQ

directions :: [SquareDirection]
directions = [North, East, South, West]

data Step = Step
  { level :: Level
  , -- The "tip" is the next tile that needs to have a track piece placed in it.
    -- This tile is currently empty in this step.
    tip :: Position
  , -- The direction we're facing after placing the previous track piece. If we
    -- last placed an EW track piece moving rightwards, we came from the West
    -- and would be "facing" East.
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
  "Facing: " ++ show facing ++ "\n"
{- FOURMOLU_ENABLE -}

solve :: Level -> Level
solve startLevel@Level{start, finish} = case solution of
  Nothing -> error "Impossible: no solution found"
  Just l -> l
 where
  -- We begin facing east, with the tip at the start tile, with no critter in
  -- the train.
  initial :: Step
  initial =
    -- debugStep
    --   "Start"
    Step
      { level = startLevel
      , tip = start
      , facing = Direction East
      , train = Nothing
      }

  -- We have reached our destination if all of the following are true:
  --
  -- 1. We have reached the finish tile.
  -- 2. Every critter has been delivered to its house.
  --
  -- We can now trivially complete the track by connecting the finish tile to
  -- the East.
  found :: Step -> Bool
  found s@Step{level = Level{tiles}, tip} =
    tip == finish && all delivered (elems tiles)
  -- debugStep' "Testing" s $ tip == finish && all delivered (elems tiles)

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
      { level = currentLevel@Level{tiles = currentTiles, grid = Grid grid}
      , tip = currentTip
      , facing = Direction facing
      } = nextSteps
     where
      -- } = debugNexts nextSteps

      debugNexts :: [Step] -> [Step]
      debugNexts nexts = trace msg nexts
       where
        msg =
          debugStepMsg "Current" currentStep
            ++ "\n"
            ++ "Nexts: "
            ++ show (length nexts)
            ++ "\n"
            ++ concat [debugStepMsg ("Next " <> show i) n | (i, n) <- zip ([1 ..] :: [Int]) nexts]

      nextPositions :: [Step]
      nextPositions = do
        -- Special case: if the current tip is the end tile, but objectives are
        -- still incomplete, then this track is obviously impossible.
        guard $ not (currentTip == finish && not (all delivered (elems currentTiles)))
        -- Choose a direction to go in.
        direction <- directions
        -- Make sure the next position is still within the grid.
        position <- maybeToList $ neighbour grid currentTip direction
        -- Make sure the next position is unoccupied.
        guard $ not $ member position currentTiles

        pure
          currentStep
            { level = currentLevel{tiles = insert currentTip (Track $ connect facing direction) currentTiles}
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

      -- TODO:
      -- There are always exactly zero, one, or two critters to pick up
      -- Can't be more than 2 because need entrance and exit rail tiles
      -- 2 critters causes collision (no pickup)
      -- 2 houses causes rightwards one to be chosen always? what about houses in a corner?

      nextSteps :: [Step]
      nextSteps = do
        -- Take a step into the next position.
        step@Step{level = level@Level{tiles}, train} <- nextPositions
        -- step@Step{level = level@Level{tiles}, train} <- debugLen "nextPositions" nextPositions

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
    let finalStep@Step{tip, facing = Direction facing, level = Level{tiles}} = last steps'
    pure $ finalStep.level{tiles = insert tip (Track $ connect facing East) tiles}

-- @connect a b@ computes the track piece that connects an existing tile facing
-- @a@ to a new tile in the direction of @b@.
--
-- This function is partial. Passing an invalid direction (i.e. one that implies
-- a track piece that doubles back on itself) will result in an error.
connect :: SquareDirection -> SquareDirection -> Orientation
connect facing nextDirection = case facing of
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
