{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import qualified Control.Foldl                     as F
import           Control.Monad.Trans.State.Strict  (State)
import qualified Control.Monad.Trans.State.Strict  as State
import           Data.Foldable                     (traverse_)
import qualified Data.Graph.Inductive.Basic        as GB
import qualified Data.Graph.Inductive.Graph        as G
import           Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.Graph.Inductive.Query.DFS    as DFS
import qualified Data.GraphViz                     as GraphViz
import           Data.GraphViz.Attributes          (Shape (BoxShape), shape)
import           Data.GraphViz.Attributes.Complete (Attribute (Label, RankDir),
                                                    Label (StrLabel),
                                                    RankDir (FromLeft))
import qualified Data.GraphViz.Commands            as GvCmd
import           Data.List                         (intercalate)
import           Data.Map.Strict                   (Map)
import qualified Data.Map.Strict                   as Map
import qualified Data.Text                         as Text
import qualified Data.Text.Lazy                    as LText
import           Terminal                          (Item (..), pickAnItem)
import           Turtle                            hiding (f, g, sortOn)

-- TODO: option to filter nodes to exclude external packages

main :: IO ()
main = do
  fcg <- buildFunctionCallGraph
  reportSize fcg
  mode <- pickAnItem browsingModes
  terminalUI fcg mode


drawInCanvas :: Gr Decl () -> IO ()
drawInCanvas gr =
  let dotGr = GraphViz.graphToDot gvParams gr
  in GvCmd.runGraphvizCanvas' dotGr GvCmd.Xlib

showReverseDependencies :: FunctionCallGraph -> G.Node -> IO ()
showReverseDependencies = showDfsSubgraph GB.grev

showDependencies :: FunctionCallGraph -> G.Node -> IO ()
showDependencies = showDfsSubgraph id

showDfsSubgraph :: (Graph -> Graph) -> FunctionCallGraph -> G.Node -> IO ()
showDfsSubgraph preprocessGraph fcg nodeId = do
  let graph = preprocessGraph $ _graph fcg
      reachableNodeIds = DFS.dfs [nodeId] graph
      subGr = G.subgraph reachableNodeIds $ _graph fcg
  drawInCanvas subGr

gvParams :: GraphViz.GraphvizParams G.Node Decl () () Decl
gvParams = GraphViz.nonClusteredParams
  { GraphViz.fmtNode = \(_nid, decl) -> [Label . StrLabel . LText.pack $ formatNode WithoutPackage decl]
  , GraphViz.globalAttributes =
      [ GraphViz.NodeAttrs [shape BoxShape]
      , GraphViz.GraphAttrs [RankDir FromLeft]
      ]
  }

data NodeFormat
  = Full
  | FunctionName
  | FunctionNameIfInModule String
  | WithoutPackage


formatNode :: NodeFormat -> Decl -> String
formatNode fmt (p,m,f) = case fmt of
  FunctionName -> f
  FunctionNameIfInModule desiredModule ->
      if m == desiredModule then f else unlines [p, m, f]
  Full -> intercalate ":" [p, m, f]
  WithoutPackage -> unlines [m,f]

type Decl = (String, String, String)
type Edge = (Decl, Decl)

loadEdges :: Shell Edge
-- TODO: mfilter (\((p,_,_),(q,_,_)) -> p == "author/project" && q == "author/project") $
loadEdges = parseLine <$> inshell "cat *.functionUsages" empty


parseLine :: Line -> Edge
parseLine = read @Edge . Text.unpack . lineToText

processNode :: Decl -> State FunctionCallGraph ()
processNode decl =
  State.modify' $ \(FCG oldMap oldGraph) ->
    let newMap = Map.alter (Just . maybe (Map.size oldMap) id) decl oldMap
        nodeId = newMap Map.! decl
        newGraph = if G.gelem nodeId oldGraph then oldGraph else G.insNode (nodeId, decl) oldGraph
    in FCG newMap newGraph

processEdges :: Edge -> State FunctionCallGraph ()
processEdges (a,b) = do
  processNode a
  processNode b
  State.modify' $ \(FCG m g) ->
    let fromId = m Map.! a
        toId = m Map.! b
        newGraph = G.insEdge (fromId, toId, ()) g
    in FCG m newGraph

data FunctionCallGraph = FCG
  { _labelToNode :: Map Decl G.Node
  , _graph       :: Graph
  } deriving Show

type Graph = Gr Decl ()

buildFunctionCallGraph :: IO FunctionCallGraph
buildFunctionCallGraph = do
  stringEdges <- fold loadEdges F.list
  pure $ State.execState (traverse_ processEdges stringEdges) (FCG Map.empty G.empty)


-- TERMINAL UI STUFF

green, red, reset :: String
green = "\ESC[32m"
red = "\ESC[31m"
reset = "\ESC[0m"

cliPrompt, cliWarn :: String -> IO ()
cliPrompt msg = putStrLn $ green <> msg <> reset
cliWarn msg = putStrLn $ red <> msg <> reset

reportSize :: FunctionCallGraph -> IO ()
reportSize fcg =
  let g = _graph fcg
  in printf ("Loaded function call graph with "%s%" nodes and "%s%" edges.\n") (repr (G.order g)) (repr (G.size g))

terminalUI :: FunctionCallGraph -> BrowsingMode -> IO ()
terminalUI fcg mode = case mode of
    Everything          -> drawInCanvas $ _graph fcg
    Dependencies        -> forever (loop showDependencies)
    ReverseDependencies -> forever (loop showReverseDependencies)
  where

    loop showDeps = do
      cliPrompt "Enter name of function:"
      searchedFunction <- getLine
      let matchingMap = Map.filterWithKey (\(_p,_m,f) _nid -> searchedFunction == f) $ _labelToNode fcg
      case Map.toList matchingMap of
        [] -> do
          cliWarn $ "Sorry, there's no function named '" <> searchedFunction <> "'! Try again."
        [(_decl, nodeId)] -> do
          showDeps fcg nodeId
        matches -> do
          cliPrompt $ "There are multiple functions named like this. Which one do you want?"
          (_, nodeId) <- pickAnItem matches
          showDeps fcg nodeId

data BrowsingMode = Everything | Dependencies | ReverseDependencies

browsingModes :: [BrowsingMode]
browsingModes = [Everything, Dependencies, ReverseDependencies]

instance Item BrowsingMode where
  showItem Everything          = "Everything"
  showItem Dependencies        = "Dependencies"
  showItem ReverseDependencies = "Reverse Dependencies"

instance Item (Decl, G.Node) where
  showItem (decl, _) = formatNode Full decl
