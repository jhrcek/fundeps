{-# LANGUAGE OverloadedStrings #-}

module Terminal.Commands
  ( Command (..),
    ExportFormat (..),
    QueryItem (..),
    CommandParseError (..),
    parseCommand,
    showHelp,
    typeHelp,
    commandSuggestions,
  )
where

import Data.Bifunctor (first)
import Data.Char (isAlphaNum)
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Text.Parsec as Parsec
import Text.Parsec ((<|>))
import qualified Text.Parsec.Char as P
import qualified Text.Parsec.Combinator as P
import Text.Parsec.Text (Parser)

typeHelp :: Text
typeHelp = "Type :help to get a list of available commands"

newtype CommandParseError = CommandParseError Text deriving (Show, Eq)

parseCommand :: Text -> Either CommandParseError Command
parseCommand input =
  first (CommandParseError . Text.pack . show) $
    Parsec.parse commandParser (Text.unpack input) input

data Command
  = Query [QueryItem]
  | Export ExportFormat [QueryItem]
  | ShowGraph
  | ShowHelp
  | EditSettings
  | Quit
  deriving (Show, Eq)

data QueryItem
  = PkgModFun Text Text Text
  | ModFun Text Text
  | Fun Text
  deriving (Show, Eq)

data ExportFormat
  = Svg
  | DotSource
  deriving (Show, Eq)

commandParser :: Parser Command
commandParser =
  ( ( P.char ':'
        *> P.choice
          [ symbol "help" $> ShowHelp,
            (symbol "q" *> (symbol "uit" <|> pure ())) $> Quit,
            (symbol "set" *> (symbol "tings" <|> pure ())) $> EditSettings,
            symbol "graph" $> ShowGraph,
            Export <$> (symbol "export" *> exportFormat)
              <*> queryItems
          ]
    )
      <|> (Query <$> queryItems)
  )
    <* P.spaces
    <* P.eof

exportFormat :: Parser ExportFormat
exportFormat =
  P.choice
    [ symbol "dot" $> DotSource,
      symbol "svg" $> Svg
    ]

queryItems :: Parser [QueryItem]
queryItems = queryItem `P.sepBy1` (symbol ",")

queryItem :: Parser QueryItem
queryItem =
  Parsec.try (PkgModFun <$> (pkg <* P.char ':') <*> (modul <* P.char ':') <*> funct)
    <|> Parsec.try (ModFun <$> (modul <* P.char ':') <*> funct)
    <|> (Fun <$> funct)
  where
    sat = fmap Text.pack . P.many1 . P.satisfy
    pkg = sat $ \c -> isAlphaNum c || c == '/' || c == '-'
    modul = sat $ \c -> isAlphaNum c || c == '.' || c == '-'
    funct = sat $ \c -> isAlphaNum c || c == '_'

symbol :: String -> Parser ()
symbol s = lexeme (P.string s) $> ()

lexeme :: Parser a -> Parser a
lexeme p =
  p <* P.spaces

showHelp :: IO ()
showHelp =
  Text.putStrLn $
    "COMMANDS\n\
    \  :help                      Show this help\n\
    \  <query>                    Show function call graph for one or more (comma separated) functions\n\
    \  :export FORMAT <query>     Export function call graph to file (supported FORMAT: dot|svg)\n\
    \  :graph                     Show the entire call graph\n\
    \  :set                       Adjust visualization settings\n\
    \  :quit                      Quit the program"

commandSuggestions :: [String]
commandSuggestions =
  [ ":graph",
    ":help",
    ":quit",
    ":set",
    ":export"
  ]
