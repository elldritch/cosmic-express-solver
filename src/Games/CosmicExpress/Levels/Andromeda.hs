module Games.CosmicExpress.Levels.Andromeda (levels) where

import Relude

import Games.CosmicExpress.Levels (Color (..), Grid (..), Level (..), Tile (..))
import Math.Geometry.Grid.Square (rectSquareGrid)

levels :: IntMap Level
levels = fromList $ zip [1 ..] [l1, l2, l3, l4, l5]

l1 :: Level
l1 =
  Level
    { tiles =
        fromList
          [ ((1, 4), Critter Purple False)
          , ((6, 2), House Purple False)
          ]
    , start = (0, 3)
    , finish = (7, 3)
    , grid = Grid $ rectSquareGrid 7 8
    }

l2 :: Level
l2 =
  Level
    { tiles =
        fromList
          [ ((2, 1), Critter Purple False)
          , ((2, 5), Critter Purple False)
          , ((6, 2), House Purple False)
          , ((6, 4), House Purple False)
          ]
    , start = (0, 3)
    , finish = (7, 3)
    , grid = Grid $ rectSquareGrid 7 8
    }

l3 :: Level
l3 =
  Level
    { tiles =
        fromList
          [ ((1, 1), House Orange False)
          , ((1, 5), Critter Orange False)
          , ((4, 1), Critter Purple False)
          , ((4, 5), House Purple False)
          , ((7, 1), House Orange False)
          , ((7, 5), Critter Orange False)
          ]
    , start = (0, 3)
    , finish = (8, 3)
    , grid = Grid $ rectSquareGrid 7 9
    }

l4 :: Level
l4 =
  Level
    { tiles =
        fromList
          [ ((3, 5), Critter Purple False)
          , ((4, 5), Critter Purple False)
          , ((3, 1), House Purple False)
          , ((4, 1), House Purple False)
          ]
    , start = (0, 3)
    , finish = (7, 3)
    , grid = Grid $ rectSquareGrid 7 8
    }

l5 :: Level
l5 =
  Level
    { tiles =
        fromList
          [ ((1, 5), Critter Purple False)
          , ((2, 5), House Orange False)
          , ((1, 1), Critter Orange False)
          , ((2, 1), House Purple False)
          , ((6, 5), House Purple False)
          , ((7, 5), Critter Orange False)
          , ((6, 1), House Orange False)
          , ((7, 1), Critter Purple False)
          ]
    , start = (0, 3)
    , finish = (4, 6)
    , grid = Grid $ rectSquareGrid 7 9
    }
