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
import           Data.GraphViz.Commands            (GraphvizCommand)
import qualified Data.GraphViz.Commands            as GVCmd
import           Data.Text                         (Text)
import qualified Data.Text                         as Text
import qualified Data.Text.IO                      as Text
import           Settings
import qualified Terminal.Ansi                     as Ansi
import           Turtle                            (repr)

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
  ["allow.multi.edges", value]         -> AllowMultiEdges <$> parseBool value
  ["graphviz.command", value]          -> SetGraphVizCommand <$> parseGraphVizCommand value
  ["include.external.packages", value] -> IncludeExternalPackages <$> parseBool value
  ["rank.dir", value]                  -> SetRankDir <$> parseRankDir value
  ["search.mode", value]               -> SetSearchMode <$> parseSearchMode value
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
  _    -> Left $ word <> " is not a valid RankDir. Valid values are: LR, RL, TB, BT"


parseBool :: Text -> Either Text Bool
parseBool word = case word of
  "True"  -> Right True
  "False" -> Right False
  _       -> Left $ word <> " is not a valid Bool. Valid values are: True, False"


parseSearchMode :: Text -> Either Text SearchMode
parseSearchMode word = case word of
  "Callees" -> Right Callees
  "Callers" -> Right Callers
  _         -> Left $ word <> "is not a valid search mode. Valid values are: Callees, Callers"


parseGraphVizCommand :: Text -> Either Text GraphvizCommand
parseGraphVizCommand x = case x of
  "Dot"       -> Right GVCmd.Dot
  "Neato"     -> Right GVCmd.Neato
  "TwoPi"     -> Right GVCmd.TwoPi
  "Circo"     -> Right GVCmd.Circo
  -- TODO experiment with these to find out which are worth including in the terminal UI
  -- "Fdp"       -> Right GVCmd.Fdp
  -- "Sfdp"      -> Right GVCmd.Sfdp
  -- "Osage"     -> Right GVCmd.Osage
  -- "Patchwork" -> Right GVCmd.Patchwork
  _           -> Left $ "'" <> x <> "' is not a name of valid GraphViz command. Valid values are: Dot, Neato, TwoPi, Circo" -- ", Fdp, Sfdp, Osage, Patchwork"



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
  | SetSearchMode SearchMode
  | SetGraphVizCommand GraphvizCommand
  | SetRankDir RankDir


adjustSettings :: AdjustSetting -> Settings -> Settings
adjustSettings a settings =
  case a of
    AllowMultiEdges flag         -> settings { _allowMultiEdges = flag }
    RunTransitiveReduction flag  -> settings { _transitiveReduction = flag }
    IncludeExternalPackages flag -> settings { _includeExternalPackages = flag }
    SetSearchMode mode       -> settings { _dependencyMode = mode }
    SetGraphVizCommand command   -> settings { _graphvizCommand = command }
    SetRankDir rankDir           -> settings { _rankDir = rankDir }


showSettings :: Settings -> IO ()
showSettings settings =
  Text.putStrLn $ Text.unlines
    [ "allow.multi.edges = " <> repr (_allowMultiEdges settings)
    , "dependency.mode = " <> repr (_dependencyMode settings)
    , "graphviz.command = " <> repr (_graphvizCommand settings)
    , "include.external.packages = " <> repr (_includeExternalPackages settings)
    , "rank.dir = " <> showRankDir (_rankDir settings)
    , "transitive.reduction = " <> repr (_transitiveReduction settings)
    ]


showHelp :: IO ()
showHelp = Text.putStrLn $ Text.unlines
 [ "COMMANDS"
 , "  <query>               Search function by name"
 , "  :help                 Display this help"
 , "  :quit                 Quit the program"
 , "  :show                 Display current settings"
 , "  :graph                Display the entire function dependency graph"
 , "  :set SETTING VALUE    Set given SETTING to given VALUE"
 , ""
 , "SETTINGS"
 , "  NAME                        VALID VALUES (" <> Ansi.bold "Default" <> ")  LEGEND"
 , "  allow.multi.edges           " <> Ansi.bold "True" <> "|False              Enable showing multiple edges?"
 , "  graphviz.command            " <> Ansi.bold "Dot" <> "|Neato|TwoPi|Circo   GraphViz program used to generate the graph"
 , "  include.external.packages   " <> Ansi.bold "False" <> "|True              Include functions from external packages (like elm/core)?"
 , "  rank.dir                    " <> Ansi.bold "LR" <> "|RL|TB|BT             GraphViz rank direction"
 , "  search.mode                 " <> Ansi.bold "Callers" <> "|Callees         Display callers (= functions which call X) or callees (= functions which X calls)?"
 , "  transitive.reduction        " <> Ansi.bold "False" <> "|True              Run transitive reduction on graph before displaying it?"
 ]


commandSuggestions :: [String]
commandSuggestions =
  [ ":graph"
  , ":help"
  , ":quit"
  , ":show"
  , ":set allow.multi.edges"
  , ":set search.mode"
  , ":set graphviz.command"
  , ":set include.external.packages"
  , ":set rank.dir"
  , ":set transitive.reduction"
  ]
