{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Declaration
    ( Decl (..)
    , PackageName (..)
    , ModuleName (..)
    , FunctionName (..)
    , NodeFormat (..)
    , formatNode
    )
where

import qualified Data.Text as Text

import Data.Aeson (FromJSON (parseJSON), ToJSON, ToJSONKey, withText)
import Data.Text (Text, unpack)
import TUI.Select (Item (showItem))
import Text.Read (readMaybe)


data Decl = Decl
    { declPackage :: PackageName
    , declModule :: ModuleName
    , declFunction :: FunctionName
    }
    deriving stock (Show, Eq, Ord)


instance Item Decl where
    showItem decl = Text.unpack $ formatNode PackageModuleFunction decl


formatNode :: NodeFormat -> Decl -> Text
formatNode fmt (Decl p m f) = case fmt of
    PackageModuleFunction ->
        Text.intercalate ":" $
            (if Text.null (unPackageName p) then id else (unPackageName p :))
                [unModuleName m, unFunctionName f]
    ModuleFunction -> unModuleName m <> ":" <> unFunctionName f
    Function -> unFunctionName f


data NodeFormat
    = PackageModuleFunction
    | ModuleFunction
    | Function
    deriving stock (Eq, Show, Read)


instance FromJSON NodeFormat where
    parseJSON = withText "NodeFormat" $ \t -> case readMaybe (unpack t) of
        Just nodeFormat -> pure nodeFormat
        Nothing -> fail $ unpack $ "Unsupported NodeFormat: " <> t


newtype PackageName = PackageName {unPackageName :: Text}
    deriving (Eq, Ord, Show, ToJSON, ToJSONKey) via Text


newtype ModuleName = ModuleName {unModuleName :: Text}
    deriving (Eq, Ord, Show, ToJSON, ToJSONKey) via Text


newtype FunctionName = FunctionName {unFunctionName :: Text}
    deriving (Eq, Ord, Show, ToJSON, ToJSONKey) via Text
