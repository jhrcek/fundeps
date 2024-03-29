{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module FunDeps
    ( main
    )
where

import Data.DepGraph (DepGraph (DepGraph, graph))
import Data.DepGraph qualified as DG
import Data.Graph.Inductive.Graph qualified as G
import Data.GraphViz.Commands qualified as GvCmd
import FunDeps.Server (runServer)
import Settings (defaultSettings)
import System.Environment (getArgs)
import TUI qualified
import Turtle (d, printf, (%))
import Prelude hiding (FilePath)


main :: IO ()
main = do
    GvCmd.quitWithoutGraphviz "It seems that graphviz is not installed. Please install it and try again."
    edges <- DG.loadEdgesOrDie
    let depGraph = DG.buildDepGraph edges
    reportSize depGraph
    args <- parseArgs
    case argsUiMode args of
        Cli -> TUI.terminalUI depGraph defaultSettings
        HttpServer port -> runServer port depGraph


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


reportSize :: DepGraph -> IO ()
reportSize DepGraph{graph} =
    printf ("Loaded function dependency graph with " % d % " nodes and " % d % " edges.\n") (G.noNodes graph) (G.size graph)
