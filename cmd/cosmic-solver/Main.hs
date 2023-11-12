module Main (main) where

import Relude
import Relude.Extra.Map (lookup)

import Options.Applicative (
  Parser,
  ParserInfo,
  auto,
  execParser,
  help,
  helper,
  info,
  long,
  option,
  strOption,
  value,
 )

import Games.CosmicExpress.Levels (renderLevel)
import Games.CosmicExpress.Levels.Andromeda qualified as Andromeda
import Games.CosmicExpress.Solve (solve)

data Options = Options
  { dataDir :: FilePath
  , constellation :: Text
  , level :: Int
  }

optionsP :: Parser Options
optionsP =
  Options
    <$> strOption (long "datadir" <> value "data" <> help "Path to directory where results are saved")
    <*> strOption (long "constellation" <> help "Constellation name")
    <*> option auto (long "level" <> help "Level number")

argparser :: ParserInfo Options
argparser = info (optionsP <**> helper) mempty

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  Options{dataDir, constellation, level} <- execParser argparser

  case constellation of
    "andromeda" -> do
      case level `lookup` Andromeda.levels of
        Nothing -> putStrLn "Invalid level number" >> exitFailure
        Just l -> do
          putStrLn "Puzzle:"
          putStrLn $ renderLevel l
          putStrLn "Solution:"
          putStrLn $ renderLevel $ solve l
    _ -> putStrLn "Invalid constellation name" >> exitFailure
