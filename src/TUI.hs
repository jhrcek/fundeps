{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module TUI (terminalUI) where

import qualified Data.Graph.Inductive.Graph as G
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified System.Console.Haskeline as Repl
import qualified TUI.Commands as Cmd
import qualified TUI.Settings.Editor

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Containers.ListUtils (nubOrd)
import Data.Declaration (Decl (..), FunctionName (..), NodeFormat (..), formatNode)
import Data.DepGraph (DepGraph (..))
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import FunDeps.Graphviz (GraphAction (..), runGraphAction, showDfsSubgraph)
import Settings (Settings (..))
import TUI.Ansi (cliInfo, cliWarn)
import TUI.Commands (QueryItem (..))
import TUI.Select (pickAnItem)
import Prelude hiding (FilePath)


terminalUI :: DepGraph -> Settings -> IO ()
terminalUI depGraph@DepGraph{currentPackage, graph, declToNode} settings_ = do
    Text.putStrLn Cmd.typeHelp
    let completionFunc = buildCompletionFunction depGraph
    let settingsWithCompletion = Repl.setComplete completionFunc Repl.defaultSettings
    Repl.runInputT settingsWithCompletion $ loop settings_
  where
    loop :: Settings -> Repl.InputT IO ()
    loop settings = do
        minput <- Repl.getInputLine "> "
        case minput of
            Nothing ->
                {- Nothing represents user pressing Ctrl+D with empty input -}
                liftIO $ cliInfo "Bye!"
            Just line ->
                case Cmd.parseCommand (Text.pack line) of
                    Left (Cmd.CommandParseError er) -> liftIO (cliWarn er) >> loop settings
                    Right command -> case command of
                        Cmd.Query queryItems ->
                            liftIO (processQuery DrawInCanvas queryItems) >> loop settings
                        Cmd.Export file graphvizOutput queryItems ->
                            liftIO (processQuery (ExportToFile file graphvizOutput) queryItems) >> loop settings
                        Cmd.ShowHelp ->
                            liftIO Cmd.showHelp >> loop settings
                        Cmd.ShowGraph ->
                            liftIO (runGraphAction DrawInCanvas settings currentPackage graph) >> loop settings
                        Cmd.EditSettings ->
                            liftIO (TUI.Settings.Editor.editSettings settings) >>= loop
                        Cmd.NoOp ->
                            loop settings
                        Cmd.Quit ->
                            liftIO $ cliInfo "Bye!"
              where
                processQuery :: GraphAction -> [QueryItem] -> IO ()
                processQuery graphAction queryItems = do
                    let lookupResults = lookupItems depGraph queryItems
                        disambiguate :: NonEmpty Decl -> IO G.Node
                        disambiguate = fmap (declToNode Map.!) . pickAnItem
                    unless (null lookupResults) $ case partitionLookupResults lookupResults of
                        Left notFounds -> do
                            for_ notFounds $ \item -> cliWarn $ "Didn't find function named '" <> item <> "'"
                        Right (foundIds1, ambiguous) -> do
                            foundIds2 <- traverse disambiguate ambiguous
                            showDfsSubgraph graphAction depGraph settings $ foundIds1 <> foundIds2


partitionLookupResults :: [LookupResult] -> Either [Text] ([G.Node], [NonEmpty Decl])
partitionLookupResults = foldr step (Right ([], []))
  where
    step r acc@(Left notFounds) = case r of
        NotFound t -> Left (t : notFounds)
        FoundUnique _ -> acc
        Ambiguous _ -> acc
    step r (Right (founds, ambiguous)) = case r of
        NotFound t -> Left [t]
        FoundUnique nid -> Right (nid : founds, ambiguous)
        Ambiguous a -> Right (founds, a : ambiguous)


lookupItems :: DepGraph -> [QueryItem] -> [LookupResult]
lookupItems depGraph =
    fmap (lookupFunctionId depGraph)


lookupFunctionId :: DepGraph -> QueryItem -> LookupResult
lookupFunctionId (DepGraph decls funs _ curPkg) qi =
    case qi of
        PkgModFun p m f -> lookupUnique (Decl p m f)
        ModFun m f -> lookupUnique (Decl curPkg m f)
        Fun fname -> case Map.lookup fname funs of
            Nothing -> NotFound $ unFunctionName fname
            Just declSet -> case Set.toList declSet of
                [] -> error $ "WTF?! Invariant broken: set of declarations with function name '" <> Text.unpack (unFunctionName fname) <> "'' was empty"
                [decl] -> case Map.lookup decl decls of
                    Nothing -> error $ "WTF? Invariant broken: '" <> Text.unpack (unFunctionName fname) <> "' was in function name map, but not in decl map"
                    Just nodeId -> FoundUnique nodeId
                m : ore -> Ambiguous (m :| ore)
  where
    lookupUnique decl = case Map.lookup decl decls of
        Nothing -> NotFound $ formatNode PackageModuleFunction decl
        Just nodeId -> FoundUnique nodeId


buildCompletionFunction :: DepGraph -> Repl.CompletionFunc IO
buildCompletionFunction DepGraph{declToNode, functionNameToNodes} = Repl.completeWord Nothing whitespace lookupCompletions
  where
    whitespace = [' ']
    fullyQualifiedSuggestions = Text.unpack . formatNode PackageModuleFunction <$> Map.keys declToNode
    functionNameSuggestions = Text.unpack . unFunctionName <$> Map.keys functionNameToNodes
    lookupCompletions prefix =
        pure
            . fmap Repl.simpleCompletion
            . filter (List.isPrefixOf prefix)
            . nubOrd
            $ Cmd.commandSuggestions <> fullyQualifiedSuggestions <> functionNameSuggestions


data LookupResult
    = FoundUnique G.Node
    | NotFound Text
    | Ambiguous (NonEmpty Decl)
