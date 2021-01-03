{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module FunDeps.Server (runServer) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Map.Strict as Map

import Data.Aeson (ToJSON)
import Data.ByteString (ByteString)
import Data.Declaration (Decl (..), FunctionName, ModuleName, PackageName)
import Data.FileEmbed (embedFile)
import Data.Map.Strict (Map)
import Data.Proxy (Proxy (..))
import Lucid (Html, body_, charset_, doctype_, head_, html_, lang_, meta_, script_, src_, title_)
import Network.HTTP.Media (MediaType, (//))
import Network.Wai (Application)
import Network.Wai.Handler.Warp (Port, run)
import Servant.API (Get, type (:<|>) (..), type (:>))
import Servant.API.ContentTypes (Accept (contentType), JSON, MimeRender (..))
import Servant.HTML.Lucid (HTML)
import Servant.Server (Server, serve)


runServer :: Port -> Map Decl G.Node -> IO ()
runServer port decls = do
    putStrLn $ "Running on http://localhost:" <> show port
    run port (app $ toAllDecls decls)


app :: AllDecls -> Application
app decls = serve (Proxy @FunDepsApi) (funDepsHandlers decls)


type FunDepsApi =
    Get '[HTML] (Html ()) -- index.html
        :<|> "main.js" :> Get '[JS] ByteString
        :<|> "declarations" :> Get '[JSON] AllDecls


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
funDepsHandlers decls =
    pure indexHtml
        :<|> pure elmApp
        :<|> pure decls


indexHtml :: Html ()
indexHtml = do
    doctype_
    html_ [lang_ "en"] $ do
        head_ $ do
            meta_ [charset_ "utf-8"]
            title_ "FunDeps"
            script_ [src_ "main.js"] (mempty :: String)
        body_ $ script_ "Elm.Main.init()"


elmApp :: ByteString
elmApp = $(embedFile "client/dist/main.js")


data JS


instance Accept JS where
    contentType :: Proxy JS -> MediaType
    contentType _ = "application" // "javascript"


instance MimeRender JS ByteString where
    mimeRender :: Proxy JS -> ByteString -> LBS.ByteString
    mimeRender _ = LBS.fromStrict
