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

parseCommand :: Text -> Either Text Command
parseCommand text = case Text.words text of
  (":help" : _) -> Right ShowHelp
  (":graph" : _) -> Right ShowGraph
  (":quit" : _) -> Right Quit
  (":set" : _) -> Right EditSettings
  (word : _)
    | Text.isPrefixOf ":" text -> Left $ word <> " is not a valid command. " <> typeHelp
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
    Text.unlines
      [ "COMMANDS",
        "  <query>    Search function by name",
        "  :help      Show this help",
        "  :quit      Quit the program",
        "  :graph     Show the entire function dependency graph",
        "  :set       Adjust visualization settings"
      ]

commandSuggestions :: [String]
commandSuggestions =
  [ ":graph",
    ":help",
    ":quit",
    ":set"
  ]
