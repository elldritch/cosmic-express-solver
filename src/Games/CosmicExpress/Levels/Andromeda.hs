module Games.CosmicExpress.Levels.Andromeda (l1) where

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
