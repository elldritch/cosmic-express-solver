module Games.CosmicExpress.Levels.Andromeda (l1, l2) where

import Relude

import Games.CosmicExpress.Levels (Color (..), Level (..), Tile (..))

l1 :: Level
l1 =
  Level
    { tiles =
        fromList
          [ ((1, 4), Critter Purple False)
          , ((6, 2), House Purple False)
          ]
    , start = 3
    , finish = 3
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
    , start = 3
    , finish = 3
    }
