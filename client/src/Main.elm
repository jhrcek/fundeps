module Main exposing (main)

import Browser exposing (Document)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Event
import Http
import Json.Encode as Encode
import PackageForest exposing (..)
import Settings exposing (Settings)


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model =
    { packageForest : PackageForest
    , settings : Settings
    , flags : Flags
    , graphImageSrc : Maybe String
    }


type Msg
    = GotPackageForest (Result Http.Error PackageForest)
    | GotGraphImageUrl (Result Http.Error String)
    | PackageForestMsg PackageForest.Msg
    | SettingsMsg Settings.Msg
    | GraphRequested


{-| For now flags represent just the port number on which backend is running
-}
type alias Flags =
    Int


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { packageForest = PackageForest.empty
      , settings = Settings.init
      , flags = flags
      , graphImageSrc = Nothing
      }
    , getPackageForest flags
    )


getPackageForest : Flags -> Cmd Msg
getPackageForest portNumber =
    Http.get
        { url = "http://localhost:" ++ String.fromInt portNumber ++ "/declarations"
        , expect = Http.expectJson GotPackageForest packageForestDecoder
        }


renderGraph : Flags -> Settings -> PackageForest -> Cmd Msg
renderGraph portNumber settings packageForest =
    Http.post
        { url = "http://localhost:" ++ String.fromInt portNumber ++ "/render-graph"
        , body =
            Http.jsonBody <|
                Encode.object
                    [ ( "settings", Settings.encode settings )
                    , ( "nodes", Encode.list Encode.int <| PackageForest.selectedNodes packageForest )
                    ]
        , expect = Http.expectString GotGraphImageUrl
        }


view : Model -> Document Msg
view model =
    { title = "FunDeps"
    , body = viewBody model
    }


viewBody : Model -> List (Html Msg)
viewBody model =
    [ Html.map SettingsMsg <| Settings.view model.settings
    , Html.map PackageForestMsg <| PackageForest.view model.packageForest
    , Html.button [ Event.onClick GraphRequested ] [ Html.text "Render Graph" ]
    , model.graphImageSrc
        |> Maybe.map (\imgSrc -> Html.img [ Attr.alt "Dependency Graph", Attr.src imgSrc ] [])
        |> Maybe.withDefault (Html.text "Graph Not Rendered yet")
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPackageForest result ->
            case result of
                Ok packageForest ->
                    ( { model | packageForest = packageForest }
                    , Cmd.none
                    )

                -- TODO deal with error
                Err _ ->
                    ( model, Cmd.none )

        GotGraphImageUrl result ->
            case result of
                Ok imageUrl ->
                    ( { model | graphImageSrc = Just imageUrl }, Cmd.none )

                -- TODO deal with error
                Err _ ->
                    ( model, Cmd.none )

        PackageForestMsg pfMsg ->
            let
                ( newPackageForest, pfCmd ) =
                    PackageForest.update pfMsg model.packageForest
            in
            ( { model | packageForest = newPackageForest }, Cmd.map PackageForestMsg pfCmd )

        SettingsMsg sMsg ->
            ( { model | settings = Settings.update sMsg model.settings }, Cmd.none )

        GraphRequested ->
            ( model, renderGraph model.flags model.settings model.packageForest )
