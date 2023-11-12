module Games.CosmicExpress.Levels.Andromeda (l1, l2, l3) where

import Relude

import Games.CosmicExpress.Levels (Color (..), Grid (..), Level (..), Tile (..))
import Math.Geometry.Grid.Square (rectSquareGrid)

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
