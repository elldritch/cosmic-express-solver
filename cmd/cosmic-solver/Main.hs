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
  switch,
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
  , noCache :: Bool
  }

optionsP :: Parser Options
optionsP =
  Options
    <$> strOption (long "datadir" <> value "data" <> help "Path to directory where results are saved")
    <*> strOption (long "constellation" <> help "Constellation name")
    <*> option auto (long "level" <> help "Level number")
    <*> switch (long "no-cache" <> help "Skip loading from and saving to cache")

argparser :: ParserInfo Options
argparser = info (optionsP <**> helper) mempty

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  Options{dataDir, constellation, level, noCache} <- execParser argparser

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
          maybeContents <-
            if noCache
              then pure Nothing
              else do
                createDirectoryIfMissing True folderPath
                -- If the solution has already been saved, load solution from file.
                result <- try $ readFileBS filePath
                case result of
                  Right bs -> pure $ Just bs
                  Left (_ :: IOException) -> pure Nothing
          -- Otherwise, solve and save the solution.
          solution <- case maybeContents >>= decodeStrict of
            Just saved -> pure saved
            Nothing -> do
              solution <- solve l
              unless noCache $ writeFileLBS filePath $ encode solution
              pure solution
          -- Display the results.
          putStrLn "Solution:"
          print solution
          -- putStrLn $ renderLevel solution
    _ -> putStrLn "Invalid constellation name" >> exitFailure
