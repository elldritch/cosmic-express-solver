module Games.CosmicExpress.Levels.Andromeda (levels) where

import Relude

import Games.CosmicExpress.Level (Color (..), Level (..), Position, Tile (..))

levels :: IntMap Level
levels = fromList $ zip [1 ..] [l1, l2, l3, l4, l5, l6]

board :: Int -> Int -> Map Position Tile
board rows columns = fromList [((x, y), Empty) | x <- [0 .. columns - 1], y <- [0 .. rows - 1]]

withTiles :: Map Position Tile -> [(Position, Tile)] -> Map Position Tile
withTiles m = (<> m) . fromList

house :: Color -> Tile
house c = House c False

critter :: Color -> Tile
critter c = Critter c False

l1 :: Level
l1 =
  Level
    { tiles =
        board 7 8
          `withTiles` [ ((1, 4), critter Purple)
                      , ((6, 2), house Purple)
                      ]
    , start = (0, 3)
    , finish = (7, 3)
    }

l2 :: Level
l2 =
  Level
    { tiles =
        board 7 8
          `withTiles` [ ((2, 1), critter Purple)
                      , ((2, 5), critter Purple)
                      , ((6, 2), house Purple)
                      , ((6, 4), house Purple)
                      ]
    , start = (0, 3)
    , finish = (7, 3)
    }

l3 :: Level
l3 =
  Level
    { tiles =
        board 7 9
          `withTiles` [ ((1, 1), house Orange)
                      , ((1, 5), critter Orange)
                      , ((4, 1), critter Purple)
                      , ((4, 5), house Purple)
                      , ((7, 1), house Orange)
                      , ((7, 5), critter Orange)
                      ]
    , start = (0, 3)
    , finish = (8, 3)
    }

l4 :: Level
l4 =
  Level
    { tiles =
        board 7 8
          `withTiles` [ ((3, 5), critter Purple)
                      , ((4, 5), critter Purple)
                      , ((3, 1), house Purple)
                      , ((4, 1), house Purple)
                      ]
    , start = (0, 3)
    , finish = (7, 3)
    }

l5 :: Level
l5 =
  Level
    { tiles =
        board 7 9
          `withTiles` [ ((1, 5), critter Purple)
                      , ((2, 5), house Orange)
                      , ((1, 1), critter Orange)
                      , ((2, 1), house Purple)
                      , ((6, 5), house Purple)
                      , ((7, 5), critter Orange)
                      , ((6, 1), house Orange)
                      , ((7, 1), critter Purple)
                      ]
    , start = (0, 3)
    , finish = (4, 6)
    }

l6 :: Level
l6 =
  Level
    { tiles =
        board 7 9
          `withTiles` [ ((2, 1), critter Purple)
                      , ((3, 1), critter Purple)
                      , ((2, 5), critter Orange)
                      , ((3, 5), critter Orange)
                      , ((7, 1), house Purple)
                      , ((7, 2), house Purple)
                      , ((7, 4), house Orange)
                      , ((7, 5), house Orange)
                      ]
    , start = (0, 3)
    , finish = (8, 3)
    }
