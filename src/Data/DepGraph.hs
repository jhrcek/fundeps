{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.DepGraph (
    DepGraph (..),
    ClusterLabel (..),
    Graph,
    loadEdgesOrDie,
    buildDepGraph,
) where

import qualified Control.Foldl as Fold
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import Control.Applicative (empty)
import Control.Monad.Trans.State.Strict (State)
import Data.Declaration (Decl (Decl, declPackage), FunctionName (..), ModuleName (..), PackageName (..))
import Data.Foldable (traverse_)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Turtle (Line, Shell, die, fold, inshell, lineToText, select)
import Prelude hiding (FilePath)


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
    State.modify' $ \(DepGraph decls funs graph_ pkg) ->
        let newDecls = Map.alter (Just . fromMaybe (Map.size decls)) decl decls
            newFuns = Map.insertWith (<>) fname (Set.singleton decl) funs
            nodeId = newDecls Map.! decl
            newGraph = if G.gelem nodeId graph_ then graph_ else G.insNode (nodeId, decl) graph_
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


buildDepGraph :: NonEmpty Edge -> DepGraph
buildDepGraph edges =
    State.execState
        (traverse_ processEdges edges)
        (DepGraph Map.empty Map.empty G.empty curPkg)
  where
    curPkg = declPackage . fst $ NonEmpty.head edges
