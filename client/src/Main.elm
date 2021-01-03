module Main exposing (main)

import Browser exposing (Document)
import Dict exposing (Dict)
import Html exposing (Html)
import Http
import Json.Decode as JD exposing (Decoder)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model =
    { declarations : DeclTree }


{-| Map from PackageName -> ModuleName -> FunctionName -> Declaration Node Id
-}
type DeclTree
    = DeclTree (Dict String (Dict String (Dict String Int)))


type Msg
    = GotDeclTree (Result Http.Error DeclTree)


getDeclTree : Cmd Msg
getDeclTree =
    Http.get
        { url = "http://localhost:3003/declarations"
        , expect = Http.expectJson GotDeclTree declTreeDecoder
        }


declTreeDecoder : Decoder DeclTree
declTreeDecoder =
    JD.map DeclTree <| JD.dict <| JD.dict <| JD.dict JD.int


viewDeclarationSelector : DeclTree -> Html Msg
viewDeclarationSelector (DeclTree d) =
    Dict.keys d
        |> List.map (\pkg -> Html.li [] [ Html.text pkg ])
        |> Html.ul []


init : () -> ( Model, Cmd Msg )
init _ =
    ( { declarations = DeclTree Dict.empty }, getDeclTree )


view : Model -> Document Msg
view model =
    { title = "FunDeps"
    , body = [ viewDeclarationSelector model.declarations ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotDeclTree result ->
            case result of
                Ok declTree ->
                    ( { model | declarations = declTree }, Cmd.none )

                -- TODO deal with error
                Err httpErr ->
                    ( model, Cmd.none )
