module Settings
  ( SearchMode(..)
  , Settings(..)
  , NodeFormat(..)
  , defaultSettings
  ) where

import           Data.GraphViz.Attributes.Complete (RankDir (FromLeft))
import           Data.GraphViz.Commands            (GraphvizCommand (Dot))


data Settings = Settings
   { _allowMultiEdges         :: Bool
   , _dependencyMode          :: SearchMode
   , _graphvizCommand         :: GraphvizCommand
   , _includeExternalPackages :: Bool
   , _nodeFormat              :: NodeFormat
   , _rankDir                 :: RankDir
   , _transitiveReduction     :: Bool
   }


defaultSettings :: Settings
defaultSettings = Settings
   { _allowMultiEdges = True
   , _dependencyMode = Callers
   , _graphvizCommand = Dot
   , _includeExternalPackages = False
   , _nodeFormat = WithoutPackage
   , _rankDir = FromLeft
   , _transitiveReduction = False
   }


data NodeFormat
  = Full
  | WithoutPackage


data SearchMode
  = Callees
  | Callers
  deriving Show
