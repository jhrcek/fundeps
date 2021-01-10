module Main exposing (main)

import Browser exposing (Document)
import Html exposing (Html)
import Http
import PackageForest exposing (..)
import Set exposing (Set)
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
    , selectedNodes : Set NodeId
    , flags : Flags
    }


type Msg
    = GotPackageForest (Result Http.Error PackageForest)
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
      , selectedNodes = Set.empty
      }
    , getPackageForest flags
    )


getPackageForest : Flags -> Cmd Msg
getPackageForest portNumber =
    Http.get
        { url = "http://localhost:" ++ String.fromInt portNumber ++ "/declarations"
        , expect = Http.expectJson GotPackageForest packageForestDecoder
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

        PackageForestMsg pfMsg ->
            let
                ( newPackageForest, pfCmd ) =
                    PackageForest.update pfMsg model.packageForest
            in
            ( { model | packageForest = newPackageForest }, Cmd.map PackageForestMsg pfCmd )

        SettingsMsg sMsg ->
            ( { model | settings = Settings.update sMsg model.settings }, Cmd.none )

        GraphRequested ->
            ( model, Cmd.none )
