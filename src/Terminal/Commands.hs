{-# LANGUAGE OverloadedStrings #-}

module Terminal.Commands
  ( Command (..),
    parseCommand,
    showHelp,
    typeHelp,
    commandSuggestions,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

typeHelp :: Text
typeHelp = "Type :help to get a list of available commands"

-- TODO switch to normal parser instead of this ad-hoc text munging
parseCommand :: Text -> Either Text Command
parseCommand text = case Text.words text of
  (word : _)
    | Text.isPrefixOf ":" text -> lookupCommandByPrefix word
    | otherwise -> Right $ Query text
  [] -> Left typeHelp

data Command
  = Query Text
  | ShowGraph
  | ShowHelp
  | EditSettings
  | Quit

showHelp :: IO ()
showHelp =
  Text.putStrLn $
    "COMMANDS\n\
    \  <query>    Search function by name\n\
    \  :help      Show this help\n\
    \  :quit      Quit the program\n\
    \  :graph     Show the entire function dependency graph\n\
    \  :set       Adjust visualization settings"

lookupCommandByPrefix :: Text -> Either Text Command
lookupCommandByPrefix word = case filter (Text.isPrefixOf word) commands of
  [":graph"] -> Right ShowGraph
  [":help"] -> Right ShowHelp
  [":quit"] -> Right Quit
  [":set"] -> Right EditSettings
  _ -> Left $ word <> " is not a valid command. " <> typeHelp
  where
    commands = fmap Text.pack commandSuggestions

commandSuggestions :: [String]
commandSuggestions =
  [ ":graph",
    ":help",
    ":quit",
    ":set"
  ]
