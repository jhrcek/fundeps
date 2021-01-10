{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Settings (
    DependencyMode (..),
    Settings (..),
    clusterByModule,
    clusterByPackage,
    defaultSettings,
    rankDir,
    transitiveReduction,
    nodeFormat,
    allowMultiEdges,
    dependencyMode,
    graphvizCommand,
    includeExternalPackages,
) where

import Data.Declaration (NodeFormat (Function))
import Data.GraphViz.Attributes.Complete (RankDir (FromLeft))
import Data.GraphViz.Commands (GraphvizCommand (Dot))
import Lens.Micro.TH (makeLenses)


data DependencyMode
    = Callees
    | Callers
    deriving stock (Eq, Show)


data Settings = Settings
    { _allowMultiEdges :: Bool
    , _clusterByModule :: Bool
    , _clusterByPackage :: Bool
    , _dependencyMode :: DependencyMode
    , _graphvizCommand :: GraphvizCommand
    , _includeExternalPackages :: Bool
    , _nodeFormat :: NodeFormat
    , _rankDir :: RankDir
    , _transitiveReduction :: Bool
    }
    deriving stock (Show)


makeLenses ''Settings


defaultSettings :: Settings
defaultSettings =
    Settings
        { _allowMultiEdges = False
        , _dependencyMode = Callers
        , _clusterByModule = True
        , _clusterByPackage = True
        , _graphvizCommand = Dot
        , _includeExternalPackages = False
        , _nodeFormat = Function
        , _rankDir = FromLeft
        , _transitiveReduction = True
        }
