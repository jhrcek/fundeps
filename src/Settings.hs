{-# LANGUAGE TemplateHaskell #-}

module Settings (
    DependencyMode (..),
    Settings (..),
    NodeFormat (..),
    clusterByModule,
    defaultSettings,
    rankDir,
    transitiveReduction,
    nodeFormat,
    allowMultiEdges,
    dependencyMode,
    graphvizCommand,
    includeExternalPackages,
) where

import Data.GraphViz.Attributes.Complete (RankDir (FromLeft))
import Data.GraphViz.Commands (GraphvizCommand (Dot))
import Lens.Micro.TH (makeLenses)


data NodeFormat
    = PackageModuleFunction
    | ModuleFunction
    | Function
    deriving (Eq, Show)


data DependencyMode
    = Callees
    | Callers
    deriving (Eq, Show)


data Settings = Settings
    { _allowMultiEdges :: Bool
    , _clusterByModule :: Bool
    , _dependencyMode :: DependencyMode
    , _graphvizCommand :: GraphvizCommand
    , _includeExternalPackages :: Bool
    , -- TODO there's functional dependency (if _clusterByModule == True => _nodeFormat == Function)
      -- So the settings form should set nodeFormat to Function and disable node format subform when clusterByModule is set to True
      _nodeFormat :: NodeFormat
    , _rankDir :: RankDir
    , _transitiveReduction :: Bool
    }
    deriving (Show)


makeLenses ''Settings


defaultSettings :: Settings
defaultSettings =
    Settings
        { _allowMultiEdges = True
        , _dependencyMode = Callers
        , _clusterByModule = True
        , _graphvizCommand = Dot
        , _includeExternalPackages = False
        , _nodeFormat = ModuleFunction
        , _rankDir = FromLeft
        , _transitiveReduction = True
        }
