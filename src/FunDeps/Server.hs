{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module FunDeps.Server (runServer) where

import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Map.Strict as Map

import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON), object)
import Data.Declaration (Decl (..), FunctionName (unFunctionName), ModuleName (unModuleName), PackageName (unPackageName))
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


type FunDepsApi = "decls" :> Get '[JSON] AllDecls


newtype AllDecls = AllDecls [(Decl, G.Node)]


instance ToJSON AllDecls where
    toJSON (AllDecls decls) =
        toJSON $ declToJson <$> decls
      where
        declToJson (Decl p m f, nid) =
            object
                [ "p" .= unPackageName p
                , "m" .= unModuleName m
                , "f" .= unFunctionName f
                , "nid" .= nid
                ]


toAllDecls :: Map Decl G.Node -> AllDecls
toAllDecls = AllDecls . Map.toList


funDepsHandlers :: AllDecls -> Server FunDepsApi
funDepsHandlers = pure
