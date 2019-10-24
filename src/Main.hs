{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Control.Foldl                     as F
import           Control.Monad.Trans.State.Strict  (State)
import qualified Control.Monad.Trans.State.Strict  as State
import           Data.Foldable                     (for_, traverse_)
import qualified Data.Graph.Inductive.Basic        as GB
import qualified Data.Graph.Inductive.Graph        as G
import           Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.Graph.Inductive.Query.DFS    as DFS
import qualified Data.GraphViz                     as GraphViz
import           Data.GraphViz.Attributes          (Shape (BoxShape), shape)
import           Data.GraphViz.Attributes.Complete (Attribute (Label, RankDir),
                                                    Label (StrLabel))
import qualified Data.GraphViz.Commands            as GvCmd
import qualified Data.List                         as List
import           Data.List.NonEmpty                (NonEmpty ((:|)))
import           Data.Map.Strict                   (Map)
import qualified Data.Map.Strict                   as Map
import           Data.Maybe                        (fromMaybe)
import           Data.Set                          (Set)
import qualified Data.Set                          as Set
import qualified Data.Text                         as Text
import qualified Data.Text.IO                      as Text
import qualified Data.Text.Lazy                    as LText
import qualified System.Console.Haskeline          as Repl
import           Terminal                          (Item (..), pickAnItem)
import           Turtle                            hiding (f, g, prefix, sortOn)

import           Settings                          (DependencyMode (..),
                                                    NodeFormat (..),
                                                    Settings (..),
                                                    defaultSettings)
import qualified Terminal.Commands                 as Cmd

main :: IO ()
main = do
  fcg <- buildFunctionCallGraph
  reportSize fcg
  terminalUI fcg defaultSettings


showDfsSubgraph :: FunctionCallGraph -> Settings -> [G.Node] -> IO ()
showDfsSubgraph fcg settings nodeIds = do
  let graph = case _dependencyMode settings of
        Forward ->          _graph fcg
        Reverse -> GB.grev $ _graph fcg
      reachableNodeIds = DFS.dfs nodeIds graph
      subGraph = G.subgraph reachableNodeIds $ _graph fcg
  drawInCanvas settings (_currentPackage fcg) subGraph


drawInCanvas :: Settings -> PackageName -> Gr Decl () -> IO ()
drawInCanvas settings currentPackage graph =
  let dotGraph = graph
        & (if _includeExternalPackages settings then id else G.labfilter ((currentPackage == ) . _decl_package))
        & GraphViz.graphToDot (gvParams settings)
        & GraphViz.setStrictness (not $ _allowMultiEdges settings)
  in GvCmd.runGraphvizCanvas' dotGraph GvCmd.Xlib


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


loadEdges :: Shell Edge
loadEdges = -- TODO this throws an exception in case no files are found
  parseEdge <$> inshell "cat *.functionUsages" empty


--TODO better representation for serializing function dependency data
parseEdge :: Line -> Edge
parseEdge line =
    ( Decl (PackageName p1) (ModuleName m1) (FunctionName f1)
    , Decl (PackageName p2) (ModuleName m2) (FunctionName f2)
    )
  where
    ((p1,m1,f1),(p2,m2,f2)) = read . Text.unpack $ lineToText line


processNode :: Decl -> State FunctionCallGraph ()
processNode decl@(Decl _ _ fname) =
  State.modify' $ \(FCG decls funs graph pkg) ->
    let newDecls = Map.alter (Just . fromMaybe (Map.size decls)) decl decls
        newFuns = Map.insertWith (<>) fname (Set.singleton decl) funs
        nodeId = newDecls Map.! decl
        newGraph = if G.gelem nodeId graph then graph else G.insNode (nodeId, decl) graph
    in FCG newDecls newFuns newGraph pkg


processEdges :: Edge -> State FunctionCallGraph ()
processEdges (a,b) = do
  processNode a
  processNode b
  State.modify' $ \(FCG decls funs g pkg) ->
    let fromId = decls Map.! a
        toId = decls Map.! b
        newGraph = G.insEdge (fromId, toId, ()) g
    in FCG decls funs newGraph pkg


data FunctionCallGraph = FCG
  { _declToNode          :: Map Decl G.Node -- ^ map declarations to the node ID used in the graph
  -- Invariant: function names in this map are exactly those that occur in the _declToNode
  , _functionNameToNodes :: Map FunctionName (Set Decl) -- ^ Map name of function to the set of declarations that have this function name
  , _graph               :: Graph
  , _currentPackage      :: PackageName -- ^ to distinguish between this and 3rd party packages
  } deriving Show


type Graph = Gr Decl ()


buildFunctionCallGraph :: IO FunctionCallGraph
buildFunctionCallGraph = do
    -- TODO it should be possible to fold into graph without materializing the whole list into memory
    edges <- fold loadEdges F.list
    currentPackage <- case edges of
          []                    -> die "TODO: No edges were loaded"
          ((Decl pkg _ _, _):_) -> pure pkg
    pure $ State.execState
        (traverse_ processEdges edges)
        (FCG Map.empty Map.empty G.empty currentPackage)

-- TERMINAL UI STUFF

green, red, reset :: Text
green = "\ESC[32m"
red = "\ESC[31m"
reset = "\ESC[0m"


cliInfo, cliWarn :: Text -> IO ()
cliInfo msg = Text.putStrLn $ green <> msg <> reset
cliWarn msg = Text.putStrLn $ red <> msg <> reset


reportSize :: FunctionCallGraph -> IO ()
reportSize fcg =
  let g = _graph fcg
  in printf ("Loaded function call graph with "%s%" nodes and "%s%" edges.\n") (repr (G.order g)) (repr (G.size g))


terminalUI :: FunctionCallGraph -> Settings -> IO ()
terminalUI fcg settings_ = do
    Text.putStrLn Cmd.typeHelp
    let completionFunc = buildCompletionFunction fcg
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
              Cmd.Query query -> liftIO (processQuery query) >> loop settings
              Cmd.ShowSettings -> liftIO (Cmd.showSettings settings) >> loop settings
              Cmd.ShowHelp -> liftIO Cmd.showHelp >> loop settings
              Cmd.ShowGraph -> liftIO (drawInCanvas settings (_currentPackage fcg) (_graph fcg)) >> loop settings
              Cmd.Quit -> liftIO (cliInfo "Bye!") >> pure ()
          where
            processQuery :: Text -> IO ()
            processQuery query = do
              let lookupResults = parseQuery fcg query
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
                  foundIds2 <- traverse (fmap (\decl -> _declToNode fcg Map.! decl) . pickAnItem) ambiguous
                  showDfsSubgraph fcg settings $ foundIds1 <> foundIds2


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


parseQuery :: FunctionCallGraph -> Text -> [LookupResult]
parseQuery fcg searchText =
    fmap (lookupFunctionId fcg) searchTerms
  where
    searchTerms = filter (not . Text.null) . fmap Text.strip $ Text.splitOn "," searchText


lookupFunctionId :: FunctionCallGraph -> Text -> LookupResult
lookupFunctionId (FCG decls funs _ _) searchText =
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

buildCompletionFunction :: FunctionCallGraph -> Repl.CompletionFunc IO
buildCompletionFunction fcg = Repl.completeWord Nothing whitespace lookupCompletions
  where
    whitespace = [] -- Empty list = everything is treated as one word to allow autocompletion of things including spaces, like ":set SETTING"

    fullyQualifiedSuggestions = fmap (Text.unpack . formatNode Full) . Map.keys $ _declToNode fcg

    functionNameSuggestions = fmap (Text.unpack . unFunctionName) . Map.keys $ _functionNameToNodes fcg

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
