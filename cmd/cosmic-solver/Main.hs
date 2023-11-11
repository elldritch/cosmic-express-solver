module Main (main) where

import Relude

import Games.CosmicExpress.Levels (renderLevel)
import Games.CosmicExpress.Levels.Andromeda qualified as Andromeda
import Games.CosmicExpress.Solve (solve)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn $ renderLevel Andromeda.l1
  putStrLn $ renderLevel $ solve Andromeda.l1
