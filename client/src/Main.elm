module Main exposing (main)

import Browser exposing (Document)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Event
import Http
import Json.Decode as JD exposing (Decoder)
import List.Extra as List
import Set exposing (Set)


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model =
    { declTree : PackageForest
    , selectedNodes : Set NodeId
    , flags : Flags
    }


type Msg
    = GotDeclTree (Result Http.Error DeclTree)
    | AllSelected
    | AllUnselected
    | PackageToggled PackageName
    | ModuleToggled PackageName ModuleName
    | FunctionToggled PackageName ModuleName FunctionName


type alias PackageForest =
    -- TODO make this opaque and move to separate module
    List PackageTree


type alias PackageTree =
    { package : PackageName
    , nodeCountTotal : Int
    , nodeCountSelected : Int -- mutable
    , modules : List ModuleTree
    }


type alias ModuleTree =
    { module_ : ModuleName
    , nodeCountTotal : Int
    , nodeCountSelected : Int -- mutable
    , functions :
        List
            { function : FunctionName
            , nodeId : NodeId
            , isSelected : Bool -- mutable
            }
    }


selectAll : PackageForest -> PackageForest
selectAll =
    List.map selectAllInPackageTree


unselectAll : PackageForest -> PackageForest
unselectAll =
    List.map unselectAllInPackageTree


selectAllInPackageTree : PackageTree -> PackageTree
selectAllInPackageTree p =
    { p
        | nodeCountSelected = p.nodeCountTotal
        , modules = List.map selectAllInModuleTree p.modules
    }


unselectAllInPackageTree : PackageTree -> PackageTree
unselectAllInPackageTree p =
    { p
        | nodeCountSelected = 0
        , modules = List.map unselectAllInModuleTree p.modules
    }


toggleAllInPackageTree : PackageTree -> PackageTree
toggleAllInPackageTree p =
    if p.nodeCountTotal == p.nodeCountSelected then
        unselectAllInPackageTree p

    else
        selectAllInPackageTree p


selectAllInModuleTree : ModuleTree -> ModuleTree
selectAllInModuleTree m =
    { m
        | nodeCountSelected = m.nodeCountTotal
        , functions = List.map (\f -> { f | isSelected = True }) m.functions
    }


unselectAllInModuleTree : ModuleTree -> ModuleTree
unselectAllInModuleTree m =
    { m
        | nodeCountSelected = 0
        , functions = List.map (\f -> { f | isSelected = False }) m.functions
    }


toggleAllInModuleTree : ModuleTree -> ModuleTree
toggleAllInModuleTree m =
    if m.nodeCountTotal == m.nodeCountSelected then
        unselectAllInModuleTree m

    else
        selectAllInModuleTree m


toggleAllInPackage : PackageName -> PackageForest -> PackageForest
toggleAllInPackage pkg =
    List.updateIf (\p -> p.package == pkg) toggleAllInPackageTree


toggleAllInModule : PackageName -> ModuleName -> PackageForest -> PackageForest
toggleAllInModule pkg mod =
    List.updateIf (\p -> p.package == pkg)
        (\p ->
            let
                newModules =
                    List.updateIf (\m -> m.module_ == mod) toggleAllInModuleTree p.modules
            in
            { p
                | modules = newModules
                , nodeCountSelected = countSelectedInModules newModules
            }
        )


toggleFunction : PackageName -> ModuleName -> FunctionName -> PackageForest -> PackageForest
toggleFunction pkg mod fun =
    List.updateIf (\p -> p.package == pkg)
        (\p ->
            let
                newModules =
                    List.updateIf (\m -> m.module_ == mod)
                        (\m ->
                            let
                                newFunctions =
                                    List.updateIf (\f -> f.function == fun)
                                        (\f -> { f | isSelected = not f.isSelected })
                                        m.functions
                            in
                            { m
                                | functions = newFunctions
                                , nodeCountSelected = List.count .isSelected newFunctions
                            }
                        )
                        p.modules
            in
            { p
                | modules = newModules
                , nodeCountSelected = countSelectedInModules newModules
            }
        )


countSelectedInModules : List ModuleTree -> Int
countSelectedInModules =
    List.sum << List.map .nodeCountSelected


buildDeclTree : Dict PackageName (Dict ModuleName (Dict FunctionName NodeId)) -> PackageForest
buildDeclTree =
    Dict.foldl
        (\package modDict packagesAcc ->
            let
                ( nodeCountTotal, modules ) =
                    Dict.foldl
                        (\module_ funDict ( pkgNodeCntAcc, modulesAcc ) ->
                            let
                                ( nodeCountInModule, functions ) =
                                    Dict.foldl
                                        (\function nid ( modNodeCntAcc, functionsAcc ) ->
                                            ( modNodeCntAcc + 1
                                            , { function = function
                                              , nodeId = nid
                                              , isSelected = True
                                              }
                                                :: functionsAcc
                                            )
                                        )
                                        ( 0, [] )
                                        funDict
                            in
                            ( pkgNodeCntAcc + nodeCountInModule
                            , { module_ = module_
                              , nodeCountTotal = nodeCountInModule
                              , nodeCountSelected = nodeCountInModule -- initially everything's selected
                              , functions = functions
                              }
                                :: modulesAcc
                            )
                        )
                        ( 0, [] )
                        modDict
            in
            { package = package
            , nodeCountTotal = nodeCountTotal
            , nodeCountSelected = nodeCountTotal -- initially everything's selected
            , modules = modules
            }
                :: packagesAcc
        )
        []


{-| For now flags represent just the port number on which backend is running
-}
type alias Flags =
    Int


{-| Map from PackageName -> ModuleName -> FunctionName -> Declaration Node Id
-}
type alias DeclTree =
    Dict PackageName (Dict ModuleName (Dict FunctionName NodeId))


type alias NodeId =
    Int


type alias PackageName =
    String


type alias ModuleName =
    String


type alias FunctionName =
    String


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { declTree = []
      , flags = flags
      , selectedNodes = Set.empty
      }
    , getDeclTree flags
    )


getDeclTree : Flags -> Cmd Msg
getDeclTree portNumber =
    Http.get
        { url = "http://localhost:" ++ String.fromInt portNumber ++ "/declarations"
        , expect = Http.expectJson GotDeclTree declTreeDecoder
        }


declTreeDecoder : Decoder DeclTree
declTreeDecoder =
    JD.dict <| JD.dict <| JD.dict JD.int


viewDeclarationSelector : PackageForest -> Html Msg
viewDeclarationSelector declTree2 =
    let
        ulist =
            Html.ul [ Attr.style "list-style-type" "none" ]
    in
    Html.div []
        [ ulist <|
            List.map
                (\p ->
                    Html.li []
                        [ Html.label [ Event.onClick (PackageToggled p.package) ]
                            [ Html.input [ Attr.type_ "checkbox", Attr.checked (p.nodeCountTotal == p.nodeCountSelected) ] []
                            , Html.text <| p.package ++ " (" ++ String.fromInt p.nodeCountSelected ++ "/" ++ String.fromInt p.nodeCountTotal ++ ")"
                            ]
                        , ulist <|
                            List.map
                                (\m ->
                                    Html.li []
                                        [ Html.label [ Event.onClick (ModuleToggled p.package m.module_) ]
                                            [ Html.input [ Attr.type_ "checkbox", Attr.checked (m.nodeCountTotal == m.nodeCountSelected) ] []
                                            , Html.text m.module_
                                            ]
                                        , ulist <|
                                            List.map
                                                (\f ->
                                                    Html.li []
                                                        [ Html.label [ Event.onClick (FunctionToggled p.package m.module_ f.function) ]
                                                            [ Html.input [ Attr.type_ "checkbox", Attr.checked f.isSelected ] []
                                                            , Html.text f.function
                                                            ]
                                                        ]
                                                )
                                                m.functions
                                        ]
                                )
                                p.modules
                        ]
                )
                declTree2
        ]


view : Model -> Document Msg
view model =
    { title = "FunDeps"
    , body = viewBody model
    }


viewBody : Model -> List (Html Msg)
viewBody model =
    [ Html.button [ Event.onClick AllSelected ] [ Html.text "Select All" ]
    , Html.button [ Event.onClick AllUnselected ] [ Html.text "Unselect All" ]
    , viewDeclarationSelector model.declTree
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotDeclTree result ->
            case result of
                Ok declTree ->
                    ( { model | declTree = buildDeclTree declTree }
                    , Cmd.none
                    )

                -- TODO deal with error
                Err _ ->
                    ( model, Cmd.none )

        AllSelected ->
            ( { model | declTree = selectAll model.declTree }, Cmd.none )

        AllUnselected ->
            ( { model | declTree = unselectAll model.declTree }, Cmd.none )

        PackageToggled packageName ->
            ( { model | declTree = toggleAllInPackage packageName model.declTree }, Cmd.none )

        ModuleToggled packageName moduleName ->
            ( { model | declTree = toggleAllInModule packageName moduleName model.declTree }, Cmd.none )

        FunctionToggled packageName moduleName functionName ->
            ( { model | declTree = toggleFunction packageName moduleName functionName model.declTree }, Cmd.none )
