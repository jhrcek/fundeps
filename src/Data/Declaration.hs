{-# LANGUAGE DerivingVia #-}

module Data.Declaration (
    Decl (..),
    PackageName (..),
    ModuleName (..),
    FunctionName (..),
    NodeFormat (..),
    formatNode,
) where

import qualified Data.Text as Text

-- import Data.List.NonEmpty (NonEmpty)
-- import Data.Map.Strict (Map)
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
        Text.unlines $
            (if Text.null (unPackageName p) then id else (unPackageName p :))
                [unModuleName m, unFunctionName f]
    ModuleFunction -> Text.unlines [unModuleName m, unFunctionName f]
    Function -> unFunctionName f


data NodeFormat
    = PackageModuleFunction
    | ModuleFunction
    | Function
    deriving (Eq, Show)


{- | All the 'Decl's we load are assumed to form a forest, where the first level
are the packages, the second are modules and the third are function names.
Selecting subset of decl's essentially means selecting a subforest of this forest.
-}

-- newtype NewQuery = NewQuery
--     (Map PackageName
--         (Map ModuleName
--             (NonEmpty FunctionName)
--         )
--     )

newtype PackageName = PackageName {unPackageName :: Text} deriving (Eq, Ord, Show) via Text


newtype ModuleName = ModuleName {unModuleName :: Text} deriving (Eq, Ord, Show) via Text


newtype FunctionName = FunctionName {unFunctionName :: Text} deriving (Eq, Ord, Show) via Text
