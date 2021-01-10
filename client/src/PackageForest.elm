module PackageForest exposing
    ( Msg
    , NodeId
    , PackageForest
    , empty
    , packageForestDecoder
    , update
    , view
    )

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Event
import Json.Decode as JD exposing (Decoder)
import List.Extra as List


type PackageForest
    = PackageForest (List PackageTree)


type alias PackageTree =
    { package : PackageName
    , expanded : Bool
    , nodeCountTotal : Int
    , nodeCountSelected : Int -- mutable
    , modules : List ModuleTree
    }


type alias ModuleTree =
    { module_ : ModuleName
    , expanded : Bool
    , nodeCountTotal : Int
    , nodeCountSelected : Int -- mutable
    , functions :
        List
            { function : FunctionName
            , nodeId : NodeId
            , isSelected : Bool -- mutable
            }
    }


type alias NodeId =
    Int


type alias PackageName =
    String


type alias ModuleName =
    String


type alias FunctionName =
    String


type Msg
    = AllSelected
    | AllUnselected
    | AllExpanded
    | AllCollapsed
    | PackageSelectionToggled PackageName
    | ModuleSelectionToggled PackageName ModuleName
    | FunctionSelectionToggled PackageName ModuleName FunctionName
    | PackageExpansionToggled PackageName
    | ModuleExpansionToggled PackageName ModuleName


{-| Helper type for easier decoding of triple nested map:
Map from PackageName -> ModuleName -> FunctionName -> Declaration Node Id
-}
type alias DeclTree =
    Dict PackageName (Dict ModuleName (Dict FunctionName NodeId))


declTreeDecoder : Decoder DeclTree
declTreeDecoder =
    JD.dict <| JD.dict <| JD.dict JD.int


packageForestDecoder : Decoder PackageForest
packageForestDecoder =
    JD.map initPackageForest declTreeDecoder


initPackageForest : DeclTree -> PackageForest
initPackageForest =
    PackageForest
        << Dict.foldl
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
                                  , expanded = False
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
                , expanded = False
                , nodeCountTotal = nodeCountTotal
                , nodeCountSelected = nodeCountTotal -- initially everything's selected
                , modules = modules
                }
                    :: packagesAcc
            )
            []


empty : PackageForest
empty =
    PackageForest []


selectAll : PackageForest -> PackageForest
selectAll =
    mapPf <| List.map selectAllInPackageTree


unselectAll : PackageForest -> PackageForest
unselectAll =
    mapPf <| List.map unselectAllInPackageTree


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
    mapPf <| List.updateIf (\p -> p.package == pkg) toggleAllInPackageTree


toggleAllInModule : PackageName -> ModuleName -> PackageForest -> PackageForest
toggleAllInModule pkg mod =
    mapPf <|
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
    mapPf <|
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


expandAll : PackageForest -> PackageForest
expandAll =
    setExpansionEverywhere True


collapseAll : PackageForest -> PackageForest
collapseAll =
    setExpansionEverywhere False


setExpansionEverywhere : Bool -> PackageForest -> PackageForest
setExpansionEverywhere b (PackageForest pf) =
    PackageForest <|
        List.map
            (\p ->
                { p
                    | expanded = b
                    , modules = List.map (\m -> { m | expanded = b }) p.modules
                }
            )
            pf


mapPf : (List PackageTree -> List PackageTree) -> (PackageForest -> PackageForest)
mapPf f (PackageForest pf) =
    PackageForest (f pf)


togglePackageExpansion : PackageName -> PackageForest -> PackageForest
togglePackageExpansion pkg =
    mapPf <| List.updateIf (\p -> p.package == pkg) (\p -> { p | expanded = not p.expanded })


toggleModuleExpansion : PackageName -> ModuleName -> PackageForest -> PackageForest
toggleModuleExpansion pkg mod =
    mapPf <|
        List.updateIf (\p -> p.package == pkg)
            (\p ->
                { p
                    | modules =
                        List.updateIf (\m -> m.module_ == mod)
                            (\m -> { m | expanded = not m.expanded })
                            p.modules
                }
            )


viewPackageForest : PackageForest -> Html Msg
viewPackageForest (PackageForest pf) =
    let
        unorderedList =
            Html.ul [ Attr.style "list-style-type" "none" ]
    in
    unorderedList <|
        List.map
            (\p ->
                Html.li []
                    [ Html.label [ Event.onClick (PackageSelectionToggled p.package) ]
                        [ Html.input [ Attr.type_ "checkbox", Attr.checked (p.nodeCountTotal == p.nodeCountSelected) ] []
                        , Html.text <| p.package ++ " (" ++ String.fromInt p.nodeCountSelected ++ "/" ++ String.fromInt p.nodeCountTotal ++ ") "
                        , Html.span [ Event.onClick (PackageExpansionToggled p.package) ]
                            [ Html.text <|
                                if p.expanded then
                                    "⊟"

                                else
                                    "⊞"
                            ]
                        ]
                    , if p.expanded then
                        unorderedList <|
                            List.map
                                (\m ->
                                    Html.li []
                                        [ Html.label [ Event.onClick (ModuleSelectionToggled p.package m.module_) ]
                                            [ Html.input [ Attr.type_ "checkbox", Attr.checked (m.nodeCountTotal == m.nodeCountSelected) ] []
                                            , Html.text m.module_
                                            , Html.span [ Event.onClick (ModuleExpansionToggled p.package m.module_) ]
                                                [ Html.text <|
                                                    if m.expanded then
                                                        "⊟"

                                                    else
                                                        "⊞"
                                                ]
                                            ]
                                        , if m.expanded then
                                            unorderedList <|
                                                List.map
                                                    (\f ->
                                                        Html.li []
                                                            [ Html.label [ Event.onClick (FunctionSelectionToggled p.package m.module_ f.function) ]
                                                                [ Html.input [ Attr.type_ "checkbox", Attr.checked f.isSelected ] []
                                                                , Html.text f.function
                                                                ]
                                                            ]
                                                    )
                                                    m.functions

                                          else
                                            Html.text ""
                                        ]
                                )
                                p.modules

                      else
                        Html.text ""
                    ]
            )
            pf


view : PackageForest -> Html Msg
view packageForest =
    Html.div []
        [ Html.button [ Event.onClick AllSelected ] [ Html.text "Select All" ]
        , Html.button [ Event.onClick AllUnselected ] [ Html.text "Unselect All" ]
        , Html.button [ Event.onClick AllExpanded ] [ Html.text "Expand All" ]
        , Html.button [ Event.onClick AllCollapsed ] [ Html.text "Collapse All" ]
        , viewPackageForest packageForest
        ]


update : Msg -> PackageForest -> ( PackageForest, Cmd Msg )
update msg packageForest =
    case msg of
        AllSelected ->
            ( selectAll packageForest, Cmd.none )

        AllUnselected ->
            ( unselectAll packageForest, Cmd.none )

        PackageSelectionToggled packageName ->
            ( toggleAllInPackage packageName packageForest, Cmd.none )

        ModuleSelectionToggled packageName moduleName ->
            ( toggleAllInModule packageName moduleName packageForest, Cmd.none )

        FunctionSelectionToggled packageName moduleName functionName ->
            ( toggleFunction packageName moduleName functionName packageForest, Cmd.none )

        AllExpanded ->
            ( expandAll packageForest, Cmd.none )

        AllCollapsed ->
            ( collapseAll packageForest, Cmd.none )

        PackageExpansionToggled packageName ->
            ( togglePackageExpansion packageName packageForest, Cmd.none )

        ModuleExpansionToggled packageName moduleName ->
            ( toggleModuleExpansion packageName moduleName packageForest, Cmd.none )
