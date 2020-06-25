{-# LANGUAGE TemplateHaskell #-}

module Settings
  ( DependencyMode (..),
    Settings (..),
    NodeFormat (..),
    defaultSettings,
    rankDir,
    transitiveReduction,
    nodeFormat,
    allowMultiEdges,
    dependencyMode,
    graphvizCommand,
    includeExternalPackages,
  )
where

import Data.GraphViz.Attributes.Complete (RankDir (FromLeft))
import Data.GraphViz.Commands (GraphvizCommand (Dot))
import Lens.Micro.TH (makeLenses)

data NodeFormat
  = PackageModuleFunction
  | ModuleFunction
  deriving (Eq, Show)

data DependencyMode
  = Callees
  | Callers
  deriving (Eq, Show)

data Settings = Settings
  { _allowMultiEdges :: Bool,
    _dependencyMode :: DependencyMode,
    _graphvizCommand :: GraphvizCommand,
    _includeExternalPackages :: Bool,
    _nodeFormat :: NodeFormat,
    _rankDir :: RankDir,
    _transitiveReduction :: Bool
  }
  deriving (Show)

makeLenses ''Settings

defaultSettings :: Settings
defaultSettings =
  Settings
    { _allowMultiEdges = True,
      _dependencyMode = Callers,
      _graphvizCommand = Dot,
      _includeExternalPackages = False,
      _nodeFormat = ModuleFunction,
      _rankDir = FromLeft,
      _transitiveReduction = False
    }
