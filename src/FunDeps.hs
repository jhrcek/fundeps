{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module FunDeps (main) where

import           Control.Concurrent                (forkIO)
import qualified Control.Foldl                     as Fold
import           Control.Monad.Trans.State.Strict  (State)
import qualified Control.Monad.Trans.State.Strict  as State
import           Data.Foldable                     (for_, traverse_)
import qualified Data.Graph.Inductive.Basic        as GB
import qualified Data.Graph.Inductive.Graph        as G
import           Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.Graph.Inductive.Query.DFS    as DFS
import qualified Data.GraphViz                     as GraphViz
import qualified Data.GraphViz.Algorithms
import           Data.GraphViz.Attributes          (Shape (BoxShape), shape)
import           Data.GraphViz.Attributes.Complete (Attribute (Label, RankDir),
                                                    Label (StrLabel))
import qualified Data.GraphViz.Commands            as GvCmd
import qualified Data.List                         as List
import           Data.List.NonEmpty                (NonEmpty ((:|)))
import qualified Data.List.NonEmpty                as NonEmpty
import           Data.Map.Strict                   (Map)
import qualified Data.Map.Strict                   as Map
import           Data.Maybe                        (fromMaybe, mapMaybe)
import           Data.Set                          (Set)
import qualified Data.Set                          as Set
import qualified Data.Text                         as Text
import qualified Data.Text.IO                      as Text
import qualified Data.Text.Lazy                    as LText
import qualified System.Console.Haskeline          as Repl
import           Terminal                          (Item (..), pickAnItem)
import           Turtle                            hiding (f, g, prefix, sortOn)

import           Settings                          (NodeFormat (..),
                                                    SearchMode (..),
                                                    Settings (..),
                                                    defaultSettings)
import qualified Terminal.Ansi                     as Ansi
import qualified Terminal.Commands                 as Cmd

main :: IO ()
main = do
  edges <- loadEdgesOrDie
  let depGraph = buildDepGraph edges
  reportSize depGraph
  terminalUI depGraph defaultSettings


showDfsSubgraph :: DepGraph -> Settings -> [G.Node] -> IO ()
showDfsSubgraph DepGraph{graph,currentPackage} settings nodeIds = do
  let reachableNodeIds = DFS.dfs nodeIds $
        case _dependencyMode settings of
          Callees -> graph
          Callers -> GB.grev graph

      subGraph = G.subgraph reachableNodeIds graph

  -- Warn about search nodes being excluded due to being from external packages
  unless (_includeExternalPackages settings) $ do
    let excludedDecls = mapMaybe (\nodeId -> do
          decl <- G.lab graph nodeId
          guard (_decl_package decl /= currentPackage)
          pure $ formatNode Full decl) nodeIds
    unless (null excludedDecls) $ do
        cliWarn "These functions were excluded from the graph, because they come from external packages:"
        traverse_ (cliWarn . (" • " <>)) excludedDecls
        cliWarn "Run `:set include.external.packages True` to include them"

  when (G.noNodes subGraph /= 0) $
      drawInCanvas settings currentPackage subGraph


drawInCanvas :: Settings -> PackageName -> Graph -> IO ()
drawInCanvas settings currentPackage graph = do
  let graph1 = excludeExternalPackages settings currentPackage graph
      dotGraph = graph1
        & GraphViz.graphToDot (gvParams settings)
        & GraphViz.setStrictness (not $ _allowMultiEdges settings)
        & removeTransitiveEdges settings
      command = _graphvizCommand settings
  printf ("Showing graph with "%d%" nodes, "%d%" edges\n") (G.noNodes graph1) (G.size graph1)
  void . forkIO $ GvCmd.runGraphvizCanvas command dotGraph GvCmd.Xlib


excludeExternalPackages :: Settings -> PackageName -> Graph -> Graph
excludeExternalPackages settings currentPackage
    | _includeExternalPackages settings = id
    | otherwise                         = G.labfilter ((currentPackage ==) . _decl_package)


removeTransitiveEdges :: Settings -> GraphViz.DotGraph G.Node -> GraphViz.DotGraph G.Node
removeTransitiveEdges settings
    | _transitiveReduction settings = Data.GraphViz.Algorithms.transitiveReduction
    | otherwise = id


gvParams :: Settings -> GraphViz.GraphvizParams G.Node Decl () () Decl
gvParams settings = GraphViz.nonClusteredParams
  { GraphViz.fmtNode = \(_nid, decl) -> [Label . StrLabel . LText.fromStrict $ formatNode (_nodeFormat settings) decl]
  , GraphViz.globalAttributes =
      [ GraphViz.NodeAttrs [shape BoxShape]
      , GraphViz.GraphAttrs [RankDir $ _rankDir settings]
      ]
  }


formatNode :: NodeFormat -> Decl -> Text
formatNode fmt (Decl p m f) = case fmt of
  Full -> Text.intercalate ":"
          $ (if Text.null (unPackageName p) then id else (unPackageName p:))
          [unModuleName m, unFunctionName f]
  WithoutPackage -> Text.unlines [unModuleName m, unFunctionName f]


data Decl = Decl
  { _decl_package  :: PackageName
  , _decl_module   :: ModuleName
  , _decl_function :: FunctionName
  } deriving (Show, Eq, Ord)

newtype PackageName  = PackageName  { unPackageName  :: Text } deriving (Eq, Ord, Show) via Text
newtype ModuleName   = ModuleName   { unModuleName   :: Text } deriving (Eq, Ord, Show) via Text
newtype FunctionName = FunctionName { unFunctionName :: Text } deriving (Eq, Ord, Show) via Text

type Edge = (Decl, Decl)


loadEdgesOrDie :: IO (NonEmpty Edge)
loadEdgesOrDie = do
  edges <- fold loadEdges Fold.list
  case NonEmpty.nonEmpty edges of
    Nothing       -> die "No edges were loaded"
    Just nonEmpty -> pure nonEmpty


loadEdges :: Shell Edge
loadEdges =
  parseEdges =<< inshell "cat *.usages" empty


parseEdges :: Line -> Shell Edge
parseEdges line = select
    [ ( Decl (PackageName p1) (ModuleName m1) (FunctionName f1)
      , Decl (PackageName p2) (ModuleName m2) (FunctionName f2)
      )
    | (p2,m2,f2) <- decls ]
  where
    ((p1,m1,f1), decls) = read . Text.unpack $ lineToText line


processNode :: Decl -> State DepGraph ()
processNode decl@(Decl _ _ fname) =
  State.modify' $ \(DepGraph decls funs graph pkg) ->
    let newDecls = Map.alter (Just . fromMaybe (Map.size decls)) decl decls
        newFuns = Map.insertWith (<>) fname (Set.singleton decl) funs
        nodeId = newDecls Map.! decl
        newGraph = if G.gelem nodeId graph then graph else G.insNode (nodeId, decl) graph
    in DepGraph newDecls newFuns newGraph pkg


processEdges :: Edge -> State DepGraph ()
processEdges (a,b) = do
  processNode a
  processNode b
  State.modify' $ \(DepGraph decls funs g pkg) ->
    let fromId = decls Map.! a
        toId = decls Map.! b
        newGraph = G.insEdge (fromId, toId, ()) g
    in DepGraph decls funs newGraph pkg


data DepGraph = DepGraph
  { declToNode          :: Map Decl G.Node -- ^ map declarations to the node ID used in the graph
  -- Invariant: function names in this map are exactly those that occur in the _declToNode
  , functionNameToNodes :: Map FunctionName (Set Decl) -- ^ Map name of function to the set of declarations that have this function name
  , graph               :: Graph
  , currentPackage      :: PackageName -- ^ to distinguish between this and 3rd party packages
  } deriving Show


type Graph = Gr Decl ()


buildDepGraph :: NonEmpty Edge -> DepGraph
buildDepGraph edges =
    State.execState
        (traverse_ processEdges edges)
        (DepGraph Map.empty Map.empty G.empty currentPackage)
  where
    currentPackage = _decl_package . fst $ NonEmpty.head edges


-- TERMINAL UI STUFF

cliInfo, cliWarn :: Text -> IO ()
cliInfo msg = Text.putStrLn $ Ansi.green msg
cliWarn msg = Text.putStrLn $ Ansi.red msg


reportSize :: DepGraph -> IO ()
reportSize DepGraph{graph} =
    printf ("Loaded function dependency graph with "%d%" nodes and "%d%" edges.\n") (G.noNodes graph) (G.size graph)


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
        Nothing -> loop settings
        Just line ->
          case Cmd.parseCommand (Text.pack line) of
            Left badCommandError -> liftIO (cliWarn badCommandError) >> loop settings
            Right command -> case command of
              Cmd.AdjustSettings change ->
                let newSettings = Cmd.adjustSettings change settings
                in loop newSettings
              Cmd.Query query  -> liftIO (processQuery query) >> loop settings
              Cmd.ShowSettings -> liftIO (Cmd.showSettings settings) >> loop settings
              Cmd.ShowHelp     -> liftIO Cmd.showHelp >> loop settings
              Cmd.ShowGraph    -> liftIO (drawInCanvas settings currentPackage graph) >> loop settings
              Cmd.Quit         -> liftIO (cliInfo "Bye!") >> pure ()
          where
            processQuery :: Text -> IO ()
            processQuery query = do
              let lookupResults = parseQuery depGraph query

                  disambiguate :: NonEmpty Decl -> IO G.Node
                  disambiguate = fmap (declToNode Map.!) . pickAnItem

              unless (null lookupResults) $ case partitionLookupResults lookupResults of
                Left (notFounds, invalidQueries) -> do
                  for_ notFounds $ \item -> cliWarn $ "Didn't find function named '" <> item <> "'"
                  for_ invalidQueries $ \q -> cliWarn $ Text.unlines
                      [ "The following was not valid query: " <> q <> "."
                      , "Valid queries have one of these forms"
                      , "  - function"
                      , "  - module:function"
                      , "  - package:module:function"
                      ]
                Right (foundIds1, ambiguous) -> do
                  foundIds2 <- traverse disambiguate ambiguous
                  showDfsSubgraph depGraph settings $ foundIds1 <> foundIds2


partitionLookupResults :: [LookupResult] -> Either ([Text],[Text]) ([G.Node], [NonEmpty Decl])
partitionLookupResults = foldr step (Right ([],[]))
  where
    step r acc@(Left (notFounds, invalidQueries)) = case r of
      InvalidQuery q -> Left (notFounds, q:invalidQueries)
      NotFound t     -> Left (t:notFounds, invalidQueries)
      FoundUnique _  -> acc
      Ambiguous _    -> acc
    step r (Right (founds, ambiguous)) = case r of
      InvalidQuery q  -> Left ([],[q])
      NotFound t      -> Left ([t], [])
      FoundUnique nid -> Right (nid:founds, ambiguous)
      Ambiguous a     -> Right (founds, a:ambiguous)


parseQuery :: DepGraph -> Text -> [LookupResult]
parseQuery depGraph searchText =
    fmap (lookupFunctionId depGraph) searchTerms
  where
    searchTerms = filter (not . Text.null) . fmap Text.strip $ Text.splitOn "," searchText


lookupFunctionId :: DepGraph -> Text -> LookupResult
lookupFunctionId (DepGraph decls funs _ _) searchText =
  case Text.splitOn ":" searchText of
    [p,m,f] -> lookupUnique (Decl (PackageName p) (ModuleName m) (FunctionName f))
    [m,f]   -> lookupUnique (Decl (PackageName "") (ModuleName m) (FunctionName f))
    [fname] -> case Map.lookup (FunctionName fname) funs of
      Nothing      -> NotFound fname
      Just declSet -> case Set.toList declSet of
        []     -> error $ "WTF?! Invariant broken: set of declarations with function name '" <> Text.unpack fname <> "'' was empty"
        [decl] -> case Map.lookup decl decls of
          Nothing     -> error $ "WTF? Invariant broken: '" <> Text.unpack fname <> "' was in function name map, but not in decl map"
          Just nodeId -> FoundUnique nodeId
        m:ore -> Ambiguous (m:|ore)
    _ -> InvalidQuery searchText
  where
    lookupUnique decl = case Map.lookup decl decls of
      Nothing     -> NotFound $ formatNode Full decl
      Just nodeId -> FoundUnique nodeId


buildCompletionFunction :: DepGraph -> Repl.CompletionFunc IO
buildCompletionFunction DepGraph{declToNode, functionNameToNodes} = Repl.completeWord Nothing whitespace lookupCompletions
  where
    whitespace = [] -- Empty list = everything is treated as one word to allow autocompletion of things including spaces, like ":set SETTING"

    fullyQualifiedSuggestions = Text.unpack . formatNode Full <$> Map.keys declToNode

    functionNameSuggestions = Text.unpack . unFunctionName <$> Map.keys functionNameToNodes

    lookupCompletions prefix = pure
      . fmap Repl.simpleCompletion
      . filter (List.isPrefixOf prefix)
      $ Cmd.commandSuggestions <> fullyQualifiedSuggestions <> functionNameSuggestions


data LookupResult
  = FoundUnique G.Node
  | InvalidQuery Text
  | NotFound Text
  | Ambiguous (NonEmpty Decl)


instance Item Decl where
  showItem decl = Text.unpack $ formatNode Full decl
