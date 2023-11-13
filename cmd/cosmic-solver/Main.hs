module Main (main) where

import Relude
import Relude.Extra.Map (lookup)

import Control.Exception (IOException, try)
import Data.Aeson (decodeStrict, encode)
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
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import Games.CosmicExpress.Level (renderLevel)
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
  let l n = fromMaybe undefined $ n `lookup` Andromeda.levels
  putStrLn $ renderLevel $ l 1
  putStrLn $ renderLevel $ l 2
  putStrLn $ renderLevel $ l 3
  putStrLn $ renderLevel $ l 4
  putStrLn $ renderLevel $ l 5
  _ <- exitSuccess

  hSetBuffering stdout NoBuffering
  Options{dataDir, constellation, level} <- execParser argparser

  case constellation of
    "andromeda" -> do
      case level `lookup` Andromeda.levels of
        Nothing -> putStrLn "Invalid level number" >> exitFailure
        Just l -> do
          -- Display the puzzle.
          putStrLn "Puzzle:"
          putStrLn $ renderLevel l
          -- Make sure directory to saved solution exists.
          let
            folderPath = dataDir </> "andromeda" </> show level
            filePath = folderPath </> "answer.json"
          createDirectoryIfMissing True folderPath
          -- If the solution has already been saved, load solution from file.
          result <- try $ readFileBS filePath
          maybeContents <- case result of
            Right bs -> pure $ Just bs
            Left (_ :: IOException) -> pure Nothing
          -- Otherwise, solve and save the solution.
          solution <- case maybeContents >>= decodeStrict of
            Just saved -> pure saved
            Nothing -> do
              let solution = solve l
              writeFileLBS filePath $ encode solution
              pure solution
          -- Display the results.
          putStrLn "Solution:"
          putStrLn $ renderLevel solution
    _ -> putStrLn "Invalid constellation name" >> exitFailure
