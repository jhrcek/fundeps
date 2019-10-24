{-# LANGUAGE OverloadedStrings #-}
module Terminal.Commands
  ( Command(..)
  , adjustSettings
  , parseCommand
  , showHelp
  , showSettings
  , typeHelp
  , commandSuggestions
  ) where

import           Data.GraphViz.Attributes.Complete (RankDir (..))
import           Data.Text                         (Text)
import qualified Data.Text                         as Text
import qualified Data.Text.IO                      as Text
import           Settings
import           Turtle                            (repr)

-- TODO: ask for confirmation when displaying too large graphs

typeHelp :: Text
typeHelp = "Type :help to get a list of available commands and settings"

parseCommand :: Text -> Either Text Command
parseCommand text = case Text.words text of
    (":help":_)    -> Right ShowHelp
    (":show":_)    -> Right ShowSettings
    (":graph":_)   -> Right ShowGraph
    (":quit":_)    -> Right Quit
    (":set":other) -> AdjustSettings <$> parseAdjustSetting other
    (word:_) | Text.isPrefixOf ":" text -> Left $ word <> " is not a valid command. " <> typeHelp
             | otherwise                -> Right $ Query text
    [] -> Left typeHelp

parseAdjustSetting :: [Text] -> Either Text AdjustSetting
parseAdjustSetting xs = case xs of
  ["include.external.packages", value] -> IncludeExternalPackages <$> parseBool value
  ["allow.multi.edges", value]         -> AllowMultiEdges <$> parseBool value
  ["rank.dir", value]                  -> SetRankDir <$> parseRankDir value
  ["dependency.mode", value]           -> SetDependencyMode <$> parseDependencyMode value
  ["transitive.reduction", value]      -> RunTransitiveReduction <$> parseBool value
  [unknown, _]                         -> Left $ "'" <> unknown <> "' is not recognized setting. " <> typeHelp
  []                                   -> Left $ "Please provide name and value of some setting. " <> typeHelp
  _                                    -> Left $ "`:set " <> Text.unwords xs <> "` didn't match valid syntax `:set SETTING VALUE`. " <> typeHelp

parseRankDir :: Text -> Either Text RankDir
parseRankDir word = case word of
  "RL" -> Right FromRight
  "LR" -> Right FromLeft
  "TB" -> Right FromTop
  "BT" -> Right FromBottom
  _    -> Left $ word <> " is not a valid RankDir. Valid values are: LR|RL|TB|BT"

parseBool :: Text -> Either Text Bool
parseBool word = case word of
  "True"  -> Right True
  "False" -> Right False
  _       -> Left $ word <> " is not a valid Bool. Valid values are: True|False"

parseDependencyMode :: Text -> Either Text DependencyMode
parseDependencyMode word = case word of
  "Forward" -> Right Forward
  "Reverse" -> Right Reverse
  _         -> Left $ word <> "is not a valid browsing mode. Valid values are: Forward|Reverse"

showRankDir :: RankDir -> Text
showRankDir rd = case rd of
    FromRight  -> "RL"
    FromLeft   -> "LR"
    FromTop    -> "TB"
    FromBottom -> "BT"

data Command
  = AdjustSettings AdjustSetting
  | Query Text
  | ShowGraph
  | ShowSettings
  | ShowHelp
  | Quit


data AdjustSetting
  = AllowMultiEdges Bool
  | RunTransitiveReduction Bool
  | IncludeExternalPackages Bool
  | SetRankDir RankDir
  | SetDependencyMode DependencyMode


adjustSettings :: AdjustSetting -> Settings -> Settings
adjustSettings a settings =
  case a of
    AllowMultiEdges flag         -> settings { _allowMultiEdges = flag }
    RunTransitiveReduction flag  -> settings { _transitiveReduction = flag }
    IncludeExternalPackages flag -> settings { _includeExternalPackages = flag }
    SetRankDir rankDir           -> settings { _rankDir = rankDir }
    SetDependencyMode mode       -> settings { _dependencyMode = mode }

showSettings :: Settings -> IO ()
showSettings settings =
  Text.putStrLn $ Text.unlines
    [ "allow.multi.edges = " <> repr (_allowMultiEdges settings)
    , "dependency.mode = " <> repr (_dependencyMode settings)
    , "include.external.packages = " <> repr (_includeExternalPackages settings)
    , "rank.dir = " <> showRankDir (_rankDir settings)
    , "transitive.reduction = " <> repr (_transitiveReduction settings)
    ]

showHelp :: IO ()
showHelp = putStrLn $ unlines
 [ "COMMANDS"
 , "  <query>               Search function by name"
 , "  :help                 Display this help"
 , "  :quit                 Quit the program"
 , "  :show                 Display current settings"
 , "  :graph                Display the entire function dependency graph"
 , "  :set SETTING VALUE    Set given SETTING to given VALUE"
 , ""
 , "SETTINGS"
 , "  SETTING                    POSSIBLE VALUES     LEGEND"
 , "  allow.multi.edges          True|False          Enable showing multiple edges"
 , "  dependency.mode            Forward|Reverse     Whether to search for dependencies of reverse dependencies"
 , "  include.external.packages  True|False          Include functions from external packages (like elm/core)?"
 , "  rank.dir                   LR|RL|TB|BT         GraphViz rank direction"
 , "  transitive.reduction       True|False          Run transitive reduction on graph before displaying it"
 ]

commandSuggestions :: [String]
commandSuggestions =
  [ ":graph"
  , ":help"
  , ":quit"
  , ":show"
  , ":set allow.multi.edges"
  , ":set dependency.mode"
  , ":set include.external.packages"
  , ":set rank.dir"
  , ":set transitive.reduction"
  ]
