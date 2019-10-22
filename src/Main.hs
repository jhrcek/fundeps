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
                                                    Label (StrLabel),
                                                    RankDir (FromTop))
import qualified Data.GraphViz.Commands            as GvCmd
import           Data.List.NonEmpty                (NonEmpty ((:|)))
import           Data.Map.Strict                   (Map)
import qualified Data.Map.Strict                   as Map
import           Data.Maybe                        (fromMaybe)
import           Data.Set                          (Set)
import qualified Data.Set                          as Set
import qualified Data.Text                         as Text
import qualified Data.Text.IO                      as Text
import qualified Data.Text.Lazy                    as LText
import           Terminal                          (Item (..), pickAnItem)
import           Turtle                            hiding (f, g, sortOn)

main :: IO ()
main = do
  fcg <- buildFunctionCallGraph
  reportSize fcg
  mode <- pickAnItem browsingModes
  terminalUI fcg mode


drawInCanvas :: GraphOptions -> Gr Decl () -> IO ()
drawInCanvas opts graph =
  let dotGraph = graph
        & (if _go_includeExternalPackages opts then id else G.labfilter (Text.null . _decl_package))
        & GraphViz.graphToDot (gvParams opts)
        & GraphViz.setStrictness (not $ _go_allowMultiEdges opts)
  in GvCmd.runGraphvizCanvas' dotGraph GvCmd.Xlib


showReverseDependencies :: FunctionCallGraph -> [G.Node] -> IO ()
showReverseDependencies = showDfsSubgraph GB.grev


showDependencies :: FunctionCallGraph -> [G.Node] -> IO ()
showDependencies = showDfsSubgraph id


showDfsSubgraph :: (Graph -> Graph) -> FunctionCallGraph -> [G.Node] -> IO ()
showDfsSubgraph preprocessGraph fcg nodeIds = do
  let graph = preprocessGraph $ _graph fcg
      reachableNodeIds = DFS.dfs nodeIds graph
      subGr = G.subgraph reachableNodeIds $ _graph fcg
  drawInCanvas defaultGraphOptions subGr


data GraphOptions = GraphOptions
   { _go_allowMultiEdges         :: Bool
   , _go_includeExternalPackages :: Bool
   , _go_rankDir                 :: RankDir
   , _go_nodeFormat              :: NodeFormat
   }


defaultGraphOptions :: GraphOptions
defaultGraphOptions = GraphOptions
   { _go_allowMultiEdges = True
   , _go_includeExternalPackages = False
   , _go_rankDir = FromTop -- FromLeft
   , _go_nodeFormat = WithoutPackage
   }


gvParams :: GraphOptions -> GraphViz.GraphvizParams G.Node Decl () () Decl
gvParams opts = GraphViz.nonClusteredParams
  { GraphViz.fmtNode = \(_nid, decl) -> [Label . StrLabel . LText.fromStrict $ formatNode (_go_nodeFormat opts) decl]
  , GraphViz.globalAttributes =
      [ GraphViz.NodeAttrs [shape BoxShape]
      , GraphViz.GraphAttrs [RankDir $ _go_rankDir opts]
      ]
  }


data NodeFormat
  = Full
  | FunctionName
  | FunctionNameIfInModule Text
  | WithoutPackage


formatNode :: NodeFormat -> Decl -> Text
formatNode fmt (Decl p m f) = case fmt of
  FunctionName -> f
  FunctionNameIfInModule desiredModule ->
      if m == desiredModule then f else Text.unlines [p, m, f]
  Full -> Text.intercalate ":" [p, m, f]
  WithoutPackage -> Text.unlines [m,f]


data Decl = Decl
  { _decl_package  :: Text
  , _decl_module   :: Text
  , _decl_function :: Text
  } deriving (Show, Eq, Ord)


type Edge = (Decl, Decl)


loadEdges :: Shell Edge
loadEdges = parseLine <$> inshell "cat *.functionUsages" empty


--TODO better representation for serializing function dependency data
parseLine :: Line -> Edge
parseLine line = (Decl p1 m1 f1, Decl  p2 m2 f2)
  where
    ((p1,m1,f1),(p2,m2,f2)) = read . Text.unpack $ lineToText line


processNode :: Decl -> State FunctionCallGraph ()
processNode decl@(Decl _ _ fname) =
  State.modify' $ \(FCG decls funs graph) ->
    let newDecls = Map.alter (Just . fromMaybe (Map.size decls)) decl decls
        newFuns = Map.insertWith (<>) fname (Set.singleton decl) funs
        nodeId = newDecls Map.! decl
        newGraph = if G.gelem nodeId graph then graph else G.insNode (nodeId, decl) graph
    in FCG newDecls newFuns newGraph


processEdges :: Edge -> State FunctionCallGraph ()
processEdges (a,b) = do
  processNode a
  processNode b
  State.modify' $ \(FCG decls funs g) ->
    let fromId = decls Map.! a
        toId = decls Map.! b
        newGraph = G.insEdge (fromId, toId, ()) g
    in FCG decls funs newGraph


data FunctionCallGraph = FCG
  { _declToNode :: Map Decl G.Node -- ^ map declarations to the node ID used in the graph
  -- Invariant: function names in this map are exactly those that occur in the _declToNode
  , _functionNameToNodes :: Map Text {-TODO newtype this string to FunctionName -} (Set Decl) -- ^ Map name of function to the set of declarations that have this function name
  , _graph      :: Graph
  } deriving Show


type Graph = Gr Decl ()


buildFunctionCallGraph :: IO FunctionCallGraph
buildFunctionCallGraph = do
    stringEdges <- fold loadEdges F.list
    pure $ State.execState
        (traverse_ processEdges stringEdges)
        (FCG Map.empty Map.empty G.empty)

-- TERMINAL UI STUFF

green, red, reset :: Text
green = "\ESC[32m"
red = "\ESC[31m"
reset = "\ESC[0m"


cliPrompt, cliWarn :: Text -> IO ()
cliPrompt msg = Text.putStrLn $ green <> msg <> reset
cliWarn msg = Text.putStrLn $ red <> msg <> reset


reportSize :: FunctionCallGraph -> IO ()
reportSize fcg =
  let g = _graph fcg
  in printf ("Loaded function call graph with "%s%" nodes and "%s%" edges.\n") (repr (G.order g)) (repr (G.size g))


terminalUI :: FunctionCallGraph -> BrowsingMode -> IO ()
terminalUI fcg mode = case mode of
    Everything          -> drawInCanvas defaultGraphOptions $ _graph fcg
    Dependencies        -> forever (loop showDependencies)
    ReverseDependencies -> forever (loop showReverseDependencies)
  where
    loop showDeps = do
      cliPrompt "Enter one or more (comma-separated) function names:"
      lookupResults <- processQuery fcg <$> Text.getLine
      unless (null lookupResults) $ case partitionLookupResults lookupResults of
        Left (notFounds, invalidQueries) -> do
          for_ notFounds $ \item -> cliWarn $ "Didn't find function named '" <> item <> "'"
          for_ invalidQueries $ \query -> cliWarn $ Text.unlines
              [ "The following was not valid query: " <> query <> "."
              , "Valid queries have one of these forms"
              , "  - function"
              , "  - module:function"
              , "  - package:module:function"
              ]
        Right (foundIds1, ambiguous) -> do
          foundIds2 <- traverse (fmap (\decl -> _declToNode fcg Map.! decl) . pickAnItem) ambiguous
          showDeps fcg $ foundIds1 <> foundIds2


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


processQuery :: FunctionCallGraph -> Text -> [LookupResult]
processQuery fcg searchText =
    fmap (lookupFunctionId fcg) searchTerms
  where
    searchTerms = filter (not . Text.null) . fmap Text.strip $ Text.splitOn "," searchText


lookupFunctionId :: FunctionCallGraph -> Text -> LookupResult
lookupFunctionId (FCG decls funs _) searchText =
  case Text.splitOn ":" searchText of
    [p,m,f] -> lookupUnique (Decl p m f)
    [m,f] -> lookupUnique (Decl "" m f)
    [fname] -> case Map.lookup fname funs of
      Nothing -> NotFound fname
      Just declSet -> case Set.toList declSet of
        [] -> error $ "WTF?! Invariant broken: set of declarations with function name '" <> Text.unpack fname <> "'' was empty"
        [decl] -> case Map.lookup decl decls of
          Nothing     -> error $ "WTF? Invariant broken: '" <> Text.unpack fname <> "' was in function name map, but not in decl map"
          Just nodeId -> FoundUnique nodeId
        m:ore -> Ambiguous (m:|ore)
    _ -> InvalidQuery searchText
  where
    lookupUnique decl = case Map.lookup decl decls of
      Nothing     -> NotFound $ formatNode Full decl
      Just nodeId -> FoundUnique nodeId


data LookupResult
  = FoundUnique G.Node
  | InvalidQuery Text
  | NotFound Text
  | Ambiguous (NonEmpty Decl)


data BrowsingMode = Everything | Dependencies | ReverseDependencies


browsingModes :: NonEmpty BrowsingMode
browsingModes = Everything :| [Dependencies, ReverseDependencies]


instance Item BrowsingMode where
  showItem Everything          = "Everything"
  showItem Dependencies        = "Dependencies"
  showItem ReverseDependencies = "Reverse Dependencies"


instance Item Decl where
  showItem decl = Text.unpack $ formatNode Full decl
