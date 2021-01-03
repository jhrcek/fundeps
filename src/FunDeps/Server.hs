{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module FunDeps.Server (runServer) where

import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Map.Strict as Map

import Data.Aeson (ToJSON)
import Data.Declaration (Decl (..), FunctionName, ModuleName, PackageName)
import Data.Map.Strict (Map)
import Data.Proxy (Proxy (..))
import Network.Wai (Application)
import Network.Wai.Handler.Warp (Port, run)
import Servant.API (Get, JSON, type (:>))
import Servant.Server (Server, serve)


runServer :: Port -> Map Decl G.Node -> IO ()
runServer port decls = do
    putStrLn $ "Running on http://localhost:" <> show port
    run port (app $ toAllDecls decls)


app :: AllDecls -> Application
app decls = serve (Proxy @FunDepsApi) (funDepsHandlers decls)


type FunDepsApi = "declarations" :> Get '[JSON] AllDecls


type DeclMap = Map PackageName (Map ModuleName (Map FunctionName G.Node))


newtype AllDecls = AllDecls DeclMap deriving (ToJSON) via DeclMap


toAllDecls :: Map Decl G.Node -> AllDecls
toAllDecls = AllDecls . go
  where
    go =
        Map.foldrWithKey'
            ( \(Decl p m f) nid pkgMap ->
                Map.alter
                    ( \mayModMap -> Just $ case mayModMap of
                        Nothing -> Map.singleton m $ Map.singleton f nid
                        Just modMap ->
                            Map.alter
                                ( \mayFunMap -> Just $ case mayFunMap of
                                    Nothing -> Map.singleton f nid
                                    Just funMap -> Map.insert f nid funMap
                                )
                                m
                                modMap
                    )
                    p
                    pkgMap
            )
            Map.empty


funDepsHandlers :: AllDecls -> Server FunDepsApi
funDepsHandlers = pure
