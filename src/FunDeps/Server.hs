{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

#ifdef WithJS
{-# LANGUAGE TemplateHaskell #-}
#endif
module FunDeps.Server (runServer) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON, parseJSON, withObject, (.:))
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Declaration (Decl (..), FunctionName, ModuleName, PackageName)
import Data.DepGraph (DepGraph (declToNode))
import Data.Graph.Inductive.Graph qualified as G
import Data.GraphViz.Commands qualified as Graphviz
import Data.List (intercalate)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text
import FunDeps.Graphviz qualified as GV
import Lucid (Html, body_, charset_, doctype_, head_, html_, lang_, meta_, script_, src_, title_)
import Network.HTTP.Media (MediaType, (//))
import Network.Wai (Application)
import Network.Wai.Handler.Warp (Port, run)
import Servant.API (Get, Post, Raw, ReqBody, type (:<|>) (..), type (:>))
import Servant.API.ContentTypes (Accept (contentType), JSON, MimeRender (..))
import Servant.HTML.Lucid (HTML)
import Servant.Server (Handler, Server, serve)
import Servant.Server.StaticFiles (serveDirectoryWebApp)
import Settings (Settings)
import Turtle (encodeString, fp, (%), (<.>), (</>))
import Turtle qualified


#ifdef WithJS
import Data.FileEmbed (embedFile)
elmApp = pure $(embedFile "client/dist/main.js")
#else
import qualified Data.ByteString (readFile)
elmApp = liftIO $ Data.ByteString.readFile "/home/jhrcek/Devel/github.com/jhrcek/fundeps/client/dist/main.js"
#endif


runServer :: Port -> DepGraph -> IO ()
runServer port depGraph = do
    putStrLn $ "Running on http://localhost:" <> show port
    Turtle.mktree imageDir
    run port $ app port depGraph


app :: Port -> DepGraph -> Application
app port depGraph =
    serve (Proxy @FunDepsApi) (funDepsHandlers port depGraph)


type FunDepsApi =
    Get '[HTML] (Html ()) -- index.html
        :<|> "main.js" :> Get '[JS] ByteString
        :<|> "declarations" :> Get '[JSON] AllDecls
        :<|> "render-graph" :> ReqBody '[JSON] RenderGraphRequest :> Post '[JSON] Text
        :<|> "static" :> Raw


data RenderGraphRequest = RenderGraphRequest
    { rgrSettings :: Settings
    , rgrNodes :: [G.Node]
    }


instance FromJSON RenderGraphRequest where
    parseJSON = withObject "RenderGraphRequest" $ \o ->
        RenderGraphRequest
            <$> o .: "settings"
            <*> o .: "nodes"


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


funDepsHandlers :: Port -> DepGraph -> Server FunDepsApi
funDepsHandlers port depGraph =
    pure (indexHtml port)
        :<|> elmApp
        :<|> pure decls
        :<|> renderGraphHandler depGraph
        :<|> serveDirectoryWebApp (encodeString imageDir)
  where
    decls = toAllDecls $ declToNode depGraph


renderGraphHandler :: DepGraph -> RenderGraphRequest -> Handler Text
renderGraphHandler depGraph RenderGraphRequest{rgrSettings, rgrNodes} = do
    utcTime <- Turtle.date
    let graphFileName = (Turtle.decodeString . intercalate "_" . take 2 . words $ show utcTime) <.> "svg"
        graphAction = GV.ExportToFile (imageDir </> graphFileName) Graphviz.Svg
    liftIO $ GV.showDfsSubgraph graphAction depGraph rgrSettings rgrNodes
    pure $ Turtle.format ("static/" % fp) graphFileName


indexHtml :: Port -> Html ()
indexHtml port = do
    doctype_
    html_ [lang_ "en"] $ do
        head_ $ do
            meta_ [charset_ "utf-8"]
            title_ "FunDeps"
            script_ [src_ "main.js"] (mempty :: String)
        body_ $ script_ $ "Elm.Main.init({flags: " <> Text.pack (show port) <> "})"


elmApp :: Handler ByteString


data JS


instance Accept JS where
    contentType :: Proxy JS -> MediaType
    contentType _ = "application" // "javascript"


instance MimeRender JS ByteString where
    mimeRender :: Proxy JS -> ByteString -> LBS.ByteString
    mimeRender _ = LBS.fromStrict


imageDir :: Turtle.FilePath
imageDir = "/tmp/fundeps"
