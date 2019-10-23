{-# LANGUAGE OverloadedStrings #-}
module Terminal.Commands
  ( Command(..)
  , adjustSettings
  , parseCommand
  , showHelp
  , showSettings
  , typeHelp
  ) where

import           Data.GraphViz.Attributes.Complete (RankDir (..))
import           Data.Text                         (Text)
import qualified Data.Text                         as Text
import           Settings

typeHelp :: Text
typeHelp = "Type :help to get a list of available commands"

parseCommand :: Text -> Either Text Command
parseCommand text = case Text.words text of
    (":help":_)    -> Right ShowHelp
    (":show":_)    -> Right ShowSettings
    (":set":other) -> AdjustSettings <$> parseAdjustSetting other
    (word:_) | Text.isPrefixOf ":" text -> Left $ word <> " is not a valid command. " <> typeHelp
             | otherwise -> Right $ Query text
    [] -> Left typeHelp

parseAdjustSetting :: [Text] -> Either Text AdjustSetting
parseAdjustSetting xs = case xs of
  ["include.external.packages", x] -> IncludeExternalPackages <$> parseBool x
  ["allow.multi.edges", x]         -> AllowMultiEdges <$> parseBool x
  ["rank.dir", x]                  -> SetRankDir <$> parseRankDir x
  ["dependency.mode", x]             -> SetDependencyMode <$> parseDependencyMode x
  _                                -> Left $ "'" <> Text.unwords xs <> "' is not recognized setting. " <> typeHelp

parseRankDir :: Text -> Either Text RankDir
parseRankDir word = case word of
  "RL" -> Right FromRight
  "LR" -> Right FromLeft
  "TB" -> Right FromTop
  "BT" -> Right FromBottom
  _    -> Left $ word <> " is not a valid RankDir. Valid values are: LR|RL|TB|BT"

parseBool :: Text -> Either Text Bool
parseBool word = case word of
  "True" -> Right True
  "False" -> Right False
  _ -> Left $ word <> " is not a valid Bool. Valid values are: True|False"

parseDependencyMode :: Text -> Either Text DependencyMode
parseDependencyMode word = case word of
  "Forward" -> Right Forward
  "Reverse" -> Right Reverse
  _ -> Left $ word <> "is not a valid browsing mode. Valid values are: Forward|Reverse"

showRankDir :: RankDir -> String
showRankDir rd = case rd of
    FromRight  -> "RL"
    FromLeft   -> "LR"
    FromTop    -> "TB"
    FromBottom -> "BT"

data Command
  = AdjustSettings AdjustSetting
  | Query Text
  | ShowSettings
  | ShowHelp


data AdjustSetting
  = AllowMultiEdges Bool
  | IncludeExternalPackages Bool
  | SetRankDir RankDir
  | SetDependencyMode DependencyMode


adjustSettings :: AdjustSetting -> Settings -> Settings
adjustSettings a settings =
  case a of
    AllowMultiEdges flag         -> settings { _allowMultiEdges = flag }
    IncludeExternalPackages flag -> settings { _includeExternalPackages = flag }
    SetRankDir rankDir           -> settings { _rankDir = rankDir }
    SetDependencyMode mode       -> settings { _dependencyMode = mode }

showSettings :: Settings -> IO ()
showSettings settings =
  putStrLn $ unlines
    [ "allow.multi.edges = " <> show (_allowMultiEdges settings)
    , "dependency.mode = " <> show (_dependencyMode settings)
    , "include.external.packages = " <> show (_includeExternalPackages settings)
    , "rank.dir = " <> showRankDir (_rankDir settings)
    ]

showHelp :: IO ()
showHelp = putStrLn $ unlines
 [ "<query>                                      name of the function to search"
 , ":help                                        Show this help"
 , ":show                                        Show current settings"
 , ":set SETTING VALUE                           Set given SETTING to given VALUE"
 , "     SETTING                    POSSIBLE VALUES     LEGEND"
 , "     allow.multi.edges          True|False          Enable showing multiple edges"
 , "     dependency.mode            Forward|Reverse     Whether to search for dependencies of reverse dependencies"
 , "     include.external.packages  True|False          Include functions from external packages (like elm/core)?"
 , "     rank.dir                   LR|RL|TB|BT         GraphViz rank direction"
 ]
