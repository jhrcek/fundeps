{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Declaration (
    Decl (..),
    PackageName (..),
    ModuleName (..),
    FunctionName (..),
    NodeFormat (..),
    formatNode,
) where

import qualified Data.Text as Text

import Data.Aeson (ToJSON, ToJSONKey)
import Data.Text (Text)
import Terminal (Item (showItem))


data Decl = Decl
    { _decl_package :: PackageName
    , _decl_module :: ModuleName
    , _decl_function :: FunctionName
    }
    deriving (Show, Eq, Ord)


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
    deriving (Eq, Show)


newtype PackageName = PackageName {unPackageName :: Text}
    deriving (Eq, Ord, Show, ToJSON, ToJSONKey) via Text


newtype ModuleName = ModuleName {unModuleName :: Text}
    deriving (Eq, Ord, Show, ToJSON, ToJSONKey) via Text


newtype FunctionName = FunctionName {unFunctionName :: Text}
    deriving (Eq, Ord, Show, ToJSON, ToJSONKey) via Text
