{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Settings
    ( DependencyMode (..)
    , Settings (..)
    , clusterByModule
    , clusterByPackage
    , defaultSettings
    , rankDir
    , transitiveReduction
    , nodeFormat
    , allowMultiEdges
    , dependencyMode
    , graphvizCommand
    , includeExternalPackages
    )
where

import Data.Aeson (FromJSON (parseJSON), withObject, withText, (.:))
import Data.Declaration (NodeFormat (Function))
import Data.GraphViz.Attributes.Complete (RankDir (FromLeft))
import Data.GraphViz.Commands (GraphvizCommand (Dot))
import Data.Text (unpack)
import Lens.Micro.TH (makeLenses)
import Text.Read (readMaybe)


data DependencyMode
    = Callees
    | Callers
    | Exact
    deriving stock (Eq, Show)


instance FromJSON DependencyMode where
    parseJSON = withText "DependencyMode" $ \t -> case t of
        "Callees" -> pure Callees
        "Callers" -> pure Callers
        "Exact" -> pure Exact
        _ -> fail $ unpack $ "Unsupported DependencyMode: " <> t


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


instance FromJSON Settings where
    parseJSON = withObject "Settings" $ \o ->
        Settings
            <$> o .: "allowMultiEdges"
            <*> o .: "clusterByModule"
            <*> o .: "clusterByPackage"
            <*> o .: "dependencyMode"
            <*> o .: "graphvizCommand"
            <*> o .: "includeExternalPackages"
            <*> o .: "nodeFormat"
            <*> o .: "rankDir"
            <*> o .: "transitiveReduction"


instance FromJSON GraphvizCommand where
    parseJSON = withText "GraphvizCommand" $ \t -> case readMaybe (unpack t) of
        Just graphvizCommand -> pure graphvizCommand
        Nothing -> fail $ unpack $ "Unsupported GraphvizCommand: " <> t


instance FromJSON RankDir where
    parseJSON = withText "RankDir" $ \t -> case readMaybe (unpack t) of
        Just rankDir -> pure rankDir
        Nothing -> fail $ unpack $ "Unsupported RankDir: " <> t


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
