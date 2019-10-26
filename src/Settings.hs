module Settings
  ( DependencyMode(..)
  , Settings(..)
  , NodeFormat(..)
  , defaultSettings
  ) where

import           Data.GraphViz.Attributes.Complete (RankDir (FromLeft))
import           Data.GraphViz.Commands            (GraphvizCommand (Dot))


data Settings = Settings
   { _allowMultiEdges         :: Bool
   , _dependencyMode          :: DependencyMode
   , _graphvizCommand         :: GraphvizCommand
   , _includeExternalPackages :: Bool
   , _nodeFormat              :: NodeFormat
   , _rankDir                 :: RankDir
   , _transitiveReduction     :: Bool
   }


defaultSettings :: Settings
defaultSettings = Settings
   { _allowMultiEdges = True
   , _dependencyMode = Reverse
   , _graphvizCommand = Dot
   , _includeExternalPackages = False
   , _nodeFormat = WithoutPackage
   , _rankDir = FromLeft
   , _transitiveReduction = False
   }


data NodeFormat
  = Full
  | WithoutPackage


data DependencyMode
  = Forward
  | Reverse
  deriving Show
