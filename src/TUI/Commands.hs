{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module TUI.Commands
    ( Command (..)
    , QueryItem (..)
    , CommandParseError (..)
    , parseCommand
    , showHelp
    , typeHelp
    , commandSuggestions
    )
where

import Data.Bifunctor (first)
import Data.Char (isAlphaNum, isSpace)
import Data.Declaration (FunctionName (..), ModuleName (..), PackageName (..))
import Data.Functor (($>))
import Data.GraphViz.Commands qualified as Gv
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Text.Parsec (parserFail, (<|>))
import Text.Parsec qualified as Parsec
import Text.Parsec.Char qualified as P
import Text.Parsec.Combinator qualified as P
import Text.Parsec.Text (Parser)
import Turtle (FilePath)
import Turtle qualified
import Prelude hiding (FilePath)


typeHelp :: Text
typeHelp = "Type :help to get a list of available commands"


newtype CommandParseError = CommandParseError Text
    deriving stock (Show, Eq)


parseCommand :: Text -> Either CommandParseError Command
parseCommand input =
    first (CommandParseError . Text.pack . show) $
        Parsec.parse commandParser (Text.unpack input) input


data Command
    = Query [QueryItem]
    | Export FilePath Gv.GraphvizOutput [QueryItem]
    | ShowGraph
    | ShowHelp
    | EditSettings
    | NoOp
    | Quit
    deriving stock (Show, Eq)


data QueryItem
    = PkgModFun PackageName ModuleName FunctionName
    | ModFun ModuleName FunctionName
    | Fun FunctionName
    deriving stock (Show, Eq)


commandParser :: Parser Command
commandParser =
    ( ( P.char ':'
            *> P.choice
                [ symbol "help" $> ShowHelp
                , (symbol "q" *> (symbol "uit" <|> pure ())) $> Quit
                , (symbol "set" *> (symbol "tings" <|> pure ())) $> EditSettings
                , symbol "graph" $> ShowGraph
                , exportCommand
                ]
      )
        <|> (Query <$> queryItems)
        <|> (NoOp <$ P.eof)
    )
        <* P.spaces
        <* P.eof


exportCommand :: Parser Command
exportCommand = do
    _ <- symbol "export"
    file <- exportFile
    graphvizOutput <- case Turtle.extension file of
        Just "dot" -> pure Gv.Canon
        Just "svg" -> pure Gv.Svg
        _ -> parserFail "Expected format of export command is :export FILE.(dot|svg) QUERY"
    Export file graphvizOutput <$> queryItems


exportFile :: Parser FilePath
exportFile = P.many1 (P.satisfy (not . isSpace)) <* P.spaces


queryItems :: Parser [QueryItem]
queryItems = queryItem `P.sepBy1` symbol ","


queryItem :: Parser QueryItem
queryItem =
    Parsec.try (PkgModFun <$> (pkg <* P.char ':') <*> (modul <* P.char ':') <*> funct)
        <|> Parsec.try (ModFun <$> (modul <* P.char ':') <*> funct)
        <|> (Fun <$> funct)
  where
    sat = fmap Text.pack . P.many1 . P.satisfy
    pkg = fmap PackageName . sat $ \c -> isAlphaNum c || c `elem` ['/', '-', '.']
    modul = fmap ModuleName . sat $ \c -> isAlphaNum c || c == '.' || c == '-'
    funct = fmap FunctionName . sat $ \c -> isAlphaNum c || c == '_'


symbol :: String -> Parser ()
symbol s = lexeme (P.string s) $> ()


lexeme :: Parser a -> Parser a
lexeme p =
    p <* P.spaces


showHelp :: IO ()
showHelp =
    Text.putStrLn
        "COMMANDS\n\
        \  :help                            Show this help\n\
        \  <query>                          Show function call graph for one or more (comma separated) functions\n\
        \                                   Query items can have the form package:module:function, module:function or function\n\
        \  :export FILE.(dot|svg) <query>   Export function call graph to file\n\
        \  :graph                           Show the entire call graph\n\
        \  :set                             Adjust visualization settings\n\
        \  :quit                            Quit the program"


commandSuggestions :: [String]
commandSuggestions =
    [ ":graph"
    , ":help"
    , ":quit"
    , ":set"
    , ":export"
    ]
