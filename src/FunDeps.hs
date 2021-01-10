{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module FunDeps (
    main,
) where

import qualified Control.Foldl as Fold
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Graph.Inductive.Basic as GB
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Query.DFS as DFS
import qualified Data.GraphViz as GV
import qualified Data.GraphViz.Algorithms
import qualified Data.GraphViz.Commands as GvCmd
import qualified Data.GraphViz.Types as GvTypes
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as LText
import qualified Settings.Editor
import qualified System.Console.Haskeline as Repl
import qualified Terminal.Ansi as Ansi
import qualified Terminal.Commands as Cmd

import Control.Concurrent (forkIO)
import Control.Monad.Trans.State.Strict (State)
import Data.Containers.ListUtils (nubOrd)
import Data.Declaration (
    Decl (Decl, declModule, declPackage),
    FunctionName (..),
    ModuleName (..),
    NodeFormat (..),
    PackageName (..),
    formatNode,
 )
import Data.Foldable (for_, traverse_)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.GraphViz.Attributes.Complete (Attribute (Label, RankDir), Label (StrLabel))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid (Endo (..))
import Data.Set (Set)
import FunDeps.Server (runServer)
import Settings (DependencyMode (..), Settings (..), defaultSettings)
import System.Environment (getArgs)
import Terminal (pickAnItem)
import Terminal.Commands (QueryItem (..))
import Turtle hiding (f, g, prefix, sortOn)
import Prelude hiding (FilePath)


main :: IO ()
main = do
    GvCmd.quitWithoutGraphviz "It seems that graphviz is not installed. Please install it and try again."
    edges <- loadEdgesOrDie
    let depGraph = buildDepGraph edges
    reportSize depGraph
    args <- parseArgs
    case argsUiMode args of
        Cli -> terminalUI depGraph defaultSettings
        HttpServer port -> runServer port (declToNode depGraph)


-- TODO proper args parsing
parseArgs :: IO Args
parseArgs = do
    args <- getArgs
    pure $
        Args $ case args of
            ["http"] -> HttpServer 3003
            _ -> Cli


newtype Args = Args {argsUiMode :: UiMode}


data UiMode = Cli | HttpServer Int


showDfsSubgraph :: GraphAction -> DepGraph -> Settings -> [G.Node] -> IO ()
showDfsSubgraph graphAction DepGraph{graph, currentPackage} settings nodeIds = do
    let reachableNodeIds = DFS.dfs nodeIds $
            case _dependencyMode settings of
                Callees -> graph
                Callers -> GB.grev graph
        subGraph = G.subgraph reachableNodeIds graph
    -- Warn about search nodes being excluded due to being from external packages
    unless (_includeExternalPackages settings) $ do
        let excludedDecls =
                mapMaybe
                    ( \nodeId -> do
                        decl <- G.lab graph nodeId
                        guard (declPackage decl /= currentPackage)
                        pure $ formatNode PackageModuleFunction decl
                    )
                    nodeIds
        unless (null excludedDecls) $ do
            cliWarn "These functions were excluded from the graph, because they come from external packages:"
            traverse_ (cliWarn . (" - " <>)) excludedDecls
    when (G.noNodes subGraph /= 0) $
        runGraphAction graphAction settings currentPackage subGraph


runGraphAction :: GraphAction -> Settings -> PackageName -> Graph -> IO ()
runGraphAction graphAction settings currentPackage graph0 = do
    let graph1 = appEndo (excludeExternalPackages settings currentPackage) graph0
        graph2 = GV.graphToDot (gvParams settings) graph1
        graph3 = GV.setStrictness (not $ _allowMultiEdges settings) graph2
        graph4 = appEndo (removeTransitiveEdges settings) graph3
        command = _graphvizCommand settings
        externalNodesExcluded = G.noNodes graph0 - G.noNodes graph1
        edgeCount2 = length $ GvTypes.graphEdges graph2
        -- `setStrictness` doesn't remove duplicate edges in the DotRepr (just sets an attribute) so we have to nub them out manually
        edgeCount3 = length . (if not (_allowMultiEdges settings) then nubOrd else id) $ GvTypes.graphEdges graph3
        edgeCount4 = length . (if not (_allowMultiEdges settings) then nubOrd else id) $ GvTypes.graphEdges graph4
        multiEdgesRemoved = edgeCount2 - edgeCount3
        transitiveEdgesRemoved = edgeCount3 - edgeCount4
    -- Log how much stuff was removed in each step
    when (externalNodesExcluded > 0) $
        if externalNodesExcluded == 1
            then printf "1 node from external package excluded. Toggle 'Include external packages' in settings include it\n"
            else printf (d % " nodes from external packages excluded. Toggle 'Include external packages' in settings to include them\n") externalNodesExcluded
    when (multiEdgesRemoved > 0) $
        printf (d % " multi edges excluded. Toggle 'Allow multi edges' in settings to include them\n") multiEdgesRemoved
    when (transitiveEdgesRemoved > 0) $
        printf (d % " transitive edges excluded. Disable 'Transitive reduction' in settings to include them\n") transitiveEdgesRemoved
    reportRendering graphAction (length $ GvTypes.graphNodes graph4) edgeCount4
    executeGraphAction command graph4 graphAction


reportRendering :: GraphAction -> Int -> Int -> IO ()
reportRendering graphAction nodeCount edgeCount =
    case graphAction of
        DrawInCanvas -> printf ("Showing graph with " % d % " nodes, " % d % " edges\n") nodeCount edgeCount
        ExportToFile file _ -> printf ("Exporting graph with " % d % " nodes, " % d % " edges to " % fp % "\n") nodeCount edgeCount file


data GraphAction
    = DrawInCanvas
    | ExportToFile FilePath GvCmd.GraphvizOutput


executeGraphAction :: GV.GraphvizCommand -> GV.DotGraph G.Node -> GraphAction -> IO ()
executeGraphAction gvCommand graph action = case action of
    DrawInCanvas -> void . forkIO $ GvCmd.runGraphvizCanvas gvCommand graph GvCmd.Xlib
    (ExportToFile file gvOutput) ->
        void $
            GvCmd.runGraphvizCommand
                gvCommand
                graph
                gvOutput
                (Turtle.encodeString file)


excludeExternalPackages :: Settings -> PackageName -> Endo Graph
excludeExternalPackages settings currentPackage
    | _includeExternalPackages settings = mempty
    | otherwise = Endo $ G.labfilter ((currentPackage ==) . declPackage)


removeTransitiveEdges :: Settings -> Endo (GV.DotGraph G.Node)
removeTransitiveEdges settings
    | _transitiveReduction settings = Endo Data.GraphViz.Algorithms.transitiveReduction
    | otherwise = mempty


gvParams :: Settings -> GV.GraphvizParams G.Node Decl () ClusterLabel Decl
gvParams settings =
    GV.defaultParams
        { GV.fmtNode = \(_nid, decl) -> [Label . StrLabel . LText.fromStrict $ formatNode (_nodeFormat settings) decl]
        , GV.clusterBy = \(nid, decl) ->
            (if _clusterByPackage settings then GV.C (PackageCluster $ declPackage decl) else id) $
                (if _clusterByModule settings then GV.C (ModuleCluster $ declModule decl) else id) $
                    GV.N (nid, decl)
        , GV.clusterID = \cl -> GV.Str . LText.fromStrict $ case cl of
            PackageCluster pkgName -> "pkg:" <> unPackageName pkgName
            ModuleCluster modName -> "mod:" <> unModuleName modName
        , GV.fmtCluster = \cl ->
            [ GV.GraphAttrs $
                case cl of
                    PackageCluster pkgName ->
                        [ Label . StrLabel . LText.fromStrict $ unPackageName pkgName
                        , GV.style GV.filled
                        , GV.fillColor GV.PowderBlue
                        ]
                    ModuleCluster modName ->
                        [ Label . StrLabel . LText.fromStrict $ unModuleName modName
                        , GV.style GV.filled
                        , GV.fillColor GV.PeachPuff
                        ]
            ]
        , GV.globalAttributes =
            [ GV.NodeAttrs
                [ GV.shape GV.BoxShape
                , GV.style GV.filled
                , GV.fillColor GV.Azure
                ]
            , GV.GraphAttrs [RankDir $ _rankDir settings]
            ]
        }


data ClusterLabel
    = PackageCluster PackageName
    | ModuleCluster ModuleName
    deriving stock (Eq, Ord)


type Edge = (Decl, Decl)


loadEdgesOrDie :: IO (NonEmpty Edge)
loadEdgesOrDie = do
    edges <- fold loadEdges Fold.list
    case NonEmpty.nonEmpty edges of
        Nothing -> die "No edges were loaded"
        Just nonEmpty -> pure nonEmpty


loadEdges :: Shell Edge
loadEdges =
    parseEdges =<< inshell "cat *.usages" empty


parseEdges :: Line -> Shell Edge
parseEdges line =
    select
        [ ( Decl (PackageName p1) (ModuleName m1) (FunctionName f1)
          , Decl (PackageName p2) (ModuleName m2) (FunctionName f2)
          )
        | (p2, m2, f2) <- decls
        ]
  where
    ((p1, m1, f1), decls) = read . Text.unpack $ lineToText line


processNode :: Decl -> State DepGraph ()
processNode decl@(Decl _ _ fname) =
    State.modify' $ \(DepGraph decls funs graph pkg) ->
        let newDecls = Map.alter (Just . fromMaybe (Map.size decls)) decl decls
            newFuns = Map.insertWith (<>) fname (Set.singleton decl) funs
            nodeId = newDecls Map.! decl
            newGraph = if G.gelem nodeId graph then graph else G.insNode (nodeId, decl) graph
         in DepGraph newDecls newFuns newGraph pkg


processEdges :: Edge -> State DepGraph ()
processEdges (a, b) = do
    processNode a
    processNode b
    State.modify' $ \(DepGraph decls funs g pkg) ->
        let fromId = decls Map.! a
            toId = decls Map.! b
            newGraph = G.insEdge (fromId, toId, ()) g
         in DepGraph decls funs newGraph pkg


data DepGraph = DepGraph
    { -- | map declarations to the node ID used in the graph
      -- Invariant: function names in this map are exactly those that occur in the _declToNode
      declToNode :: Map Decl G.Node
    , -- | Map name of function to the set of declarations that have this function name
      functionNameToNodes :: Map FunctionName (Set Decl)
    , graph :: Graph
    , -- | to distinguish between this and 3rd party packages
      -- TODO when loading stuff from multi-package hs project there might not be a single "current" package
      currentPackage :: PackageName
    }
    deriving stock (Show)


type Graph = Gr Decl ()


buildDepGraph :: NonEmpty Edge -> DepGraph
buildDepGraph edges =
    State.execState
        (traverse_ processEdges edges)
        (DepGraph Map.empty Map.empty G.empty currentPackage)
  where
    currentPackage = declPackage . fst $ NonEmpty.head edges


-- TERMINAL UI STUFF

cliInfo, cliWarn :: Text -> IO ()
cliInfo msg = Text.putStrLn $ Ansi.green msg
cliWarn msg = Text.putStrLn $ Ansi.red msg


reportSize :: DepGraph -> IO ()
reportSize DepGraph{graph} =
    printf ("Loaded function dependency graph with " % d % " nodes and " % d % " edges.\n") (G.noNodes graph) (G.size graph)


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
                            liftIO (Settings.Editor.editSettings settings) >>= loop
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
