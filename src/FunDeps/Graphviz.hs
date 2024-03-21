{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module FunDeps.Graphviz
    ( GraphAction (..)
    , showDfsSubgraph
    , runGraphAction
    )
where

import Control.Concurrent (forkIO)
import Control.Monad (guard, unless, void, when)
import Data.Containers.ListUtils (nubOrd)
import Data.Declaration (Decl (..), ModuleName (..), NodeFormat (..), PackageName (..), formatNode)
import Data.DepGraph
    ( ClusterLabel (..)
    , DepGraph (DepGraph, currentPackage, graph)
    , Graph
    )
import Data.Foldable (traverse_)
import Data.Graph.Inductive.Graph qualified as G
import Data.Graph.Inductive.Query.DFS qualified as DFS
import Data.GraphViz qualified as GV
import Data.GraphViz.Algorithms qualified
import Data.GraphViz.Attributes.Complete (Attribute (Label, RankDir), Label (StrLabel))
import Data.GraphViz.Commands qualified as GvCmd
import Data.GraphViz.Types qualified as GvTypes
import Data.Maybe (mapMaybe)
import Data.Monoid (Endo (..))
import Data.Text.Lazy qualified as LText
import Settings (DependencyMode (..), Settings (..))
import TUI.Ansi (cliWarn)
import Turtle (FilePath, d, fp, printf, (%))
import Prelude hiding (FilePath)


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
                file


-- TODO rename this as it's not always doing DFS
showDfsSubgraph :: GraphAction -> DepGraph -> Settings -> [G.Node] -> IO ()
showDfsSubgraph graphAction DepGraph{graph, currentPackage} settings nodeIds = do
    let selectedNodes = selectNodes (_dependencyMode settings) nodeIds graph
        subGraph = G.subgraph selectedNodes graph
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


selectNodes :: DependencyMode -> [G.Node] -> Graph -> [G.Node]
selectNodes dependencyMode startNodes graph =
    case dependencyMode of
        Callees -> DFS.dfs startNodes graph
        Callers -> DFS.rdfs startNodes graph
        Exact -> startNodes


reportRendering :: GraphAction -> Int -> Int -> IO ()
reportRendering graphAction nodeCount edgeCount =
    case graphAction of
        DrawInCanvas -> printf ("Showing graph with " % d % " nodes, " % d % " edges\n") nodeCount edgeCount
        ExportToFile file _ -> printf ("Exporting graph with " % d % " nodes, " % d % " edges to " % fp % "\n") nodeCount edgeCount file


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
