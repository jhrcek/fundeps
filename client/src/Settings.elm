module Settings exposing (Msg, Settings, encode, init, update, view)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Event
import Json.Encode as Encode exposing (Value)


type alias Settings =
    { allowMultiEdges : Bool
    , clusterByModule : Bool
    , clusterByPackage : Bool
    , dependencyMode : DependencyMode
    , graphvizCommand : GraphvizCommand
    , includeExternalPackages : Bool
    , nodeFormat : NodeFormat
    , rankDir : RankDir
    , transitiveReduction : Bool
    }


type Msg
    = AllowMultiEdgesToggled
    | ClusterByModuleToggled
    | ClusterByPackageToggled
    | IncludeExternalPackagesToggled
    | TransitiveReductionToggled
    | DependencyModeSelected DependencyMode
    | GraphvizCommandSelected GraphvizCommand
    | NodeFormatSelected NodeFormat
    | RankDirSelected RankDir


view : Settings -> Html Msg
view s =
    Html.div []
        [ checkbox AllowMultiEdgesToggled s.allowMultiEdges "Allow multiple edges"
        , checkbox ClusterByModuleToggled s.clusterByModule "Cluster by module"
        , checkbox ClusterByPackageToggled s.clusterByPackage "Cluster by package"
        , checkbox IncludeExternalPackagesToggled s.includeExternalPackages "Include external packages"
        , checkbox TransitiveReductionToggled s.transitiveReduction "Transitive reduction"
        , radios "Dependency Mode " "dm" DependencyModeSelected showDependencyMode s.dependencyMode [ Callers, Callees ]
        , radios "Graphviz Command" "gc" GraphvizCommandSelected showGraphvizCommand s.graphvizCommand [ Dot, Neato, TwoPi, Circo, Fdp, Sfdp, Osage, Patchwork ]
        , radios "Node Format" "nf" NodeFormatSelected showNodeFormat s.nodeFormat [ PackageModuleFunction, ModuleFunction, Function ]
        , radios "Rank Direction" "rd" RankDirSelected showRankDir s.rankDir [ FromTop, FromLeft, FromBottom, FromRight ]
        ]


checkbox : Msg -> Bool -> String -> Html Msg
checkbox onClick checked label =
    Html.div []
        [ Html.label []
            [ Html.input [ Attr.type_ "checkbox", Event.onClick onClick, Attr.checked checked ] []
            , Html.text label
            ]
        ]


radios : String -> String -> (a -> Msg) -> (a -> String) -> a -> List a -> Html Msg
radios title name toMsg toString currentVal values =
    Html.div [] <|
        Html.span [ Attr.style "width" "150px", Attr.style "float" "left" ] [ Html.text title ]
            :: List.map
                (\val ->
                    Html.label [ Event.onClick <| toMsg val ]
                        [ Html.input
                            [ Attr.type_ "radio"
                            , Attr.name name
                            , Attr.checked (val == currentVal)
                            ]
                            []
                        , Html.text <| toString val
                        ]
                )
                values


update : Msg -> Settings -> Settings
update msg s =
    case msg of
        AllowMultiEdgesToggled ->
            { s | allowMultiEdges = not s.allowMultiEdges }

        ClusterByModuleToggled ->
            { s | clusterByModule = not s.clusterByModule }

        ClusterByPackageToggled ->
            { s | clusterByPackage = not s.clusterByPackage }

        IncludeExternalPackagesToggled ->
            { s | includeExternalPackages = not s.includeExternalPackages }

        TransitiveReductionToggled ->
            { s | transitiveReduction = not s.transitiveReduction }

        DependencyModeSelected dm ->
            { s | dependencyMode = dm }

        GraphvizCommandSelected gvc ->
            { s | graphvizCommand = gvc }

        NodeFormatSelected nf ->
            { s | nodeFormat = nf }

        RankDirSelected rd ->
            { s | rankDir = rd }


init : Settings
init =
    { allowMultiEdges = False
    , dependencyMode = Callers
    , clusterByModule = True
    , clusterByPackage = True
    , graphvizCommand = Dot
    , includeExternalPackages = False
    , nodeFormat = Function
    , rankDir = FromLeft
    , transitiveReduction = True
    }


{-| <https://hackage.haskell.org/package/graphviz-2999.20.1.0/docs/Data-GraphViz-Attributes-Complete.html#t:RankDir>
-}
type RankDir
    = FromTop
    | FromLeft
    | FromBottom
    | FromRight


{-| <https://hackage.haskell.org/package/graphviz-2999.20.1.0/docs/Data-GraphViz-Commands.html#t:GraphvizCommand>
-}
type GraphvizCommand
    = Dot
    | Neato
    | TwoPi
    | Circo
    | Fdp
    | Sfdp
    | Osage
    | Patchwork


type DependencyMode
    = Callees
    | Callers


type NodeFormat
    = PackageModuleFunction
    | ModuleFunction
    | Function


encode : Settings -> Value
encode s =
    Encode.object
        [ ( "allowMultiEdges", Encode.bool s.allowMultiEdges )
        , ( "dependencyMode", encodeDependencyMode s.dependencyMode )
        , ( "clusterByModule", Encode.bool s.clusterByModule )
        , ( "clusterByPackage", Encode.bool s.clusterByPackage )
        , ( "graphvizCommand", encodeGraphvizCommand s.graphvizCommand )
        , ( "includeExternalPackages", Encode.bool s.includeExternalPackages )
        , ( "nodeFormat", encodeNodeFormat s.nodeFormat )
        , ( "rankDir", encodeRankDir s.rankDir )
        , ( "transitiveReduction", Encode.bool s.transitiveReduction )
        ]


encodeDependencyMode : DependencyMode -> Value
encodeDependencyMode =
    Encode.string << showDependencyMode


encodeGraphvizCommand : GraphvizCommand -> Value
encodeGraphvizCommand =
    Encode.string << showGraphvizCommand


encodeNodeFormat : NodeFormat -> Value
encodeNodeFormat =
    Encode.string << showNodeFormat


encodeRankDir : RankDir -> Value
encodeRankDir =
    Encode.string << showRankDir


showRankDir : RankDir -> String
showRankDir rd =
    case rd of
        FromTop ->
            "FromTop"

        FromLeft ->
            "FromLeft"

        FromBottom ->
            "FromBottom"

        FromRight ->
            "FromRight"


showDependencyMode : DependencyMode -> String
showDependencyMode dm =
    case dm of
        Callees ->
            "Callees"

        Callers ->
            "Callers"


showGraphvizCommand : GraphvizCommand -> String
showGraphvizCommand gvc =
    case gvc of
        Dot ->
            "Dot"

        Neato ->
            "Neato"

        TwoPi ->
            "TwoPi"

        Circo ->
            "Circo"

        Fdp ->
            "Fdp"

        Sfdp ->
            "Sfdp"

        Osage ->
            "Osage"

        Patchwork ->
            "Patchwork"


showNodeFormat : NodeFormat -> String
showNodeFormat nf =
    case nf of
        PackageModuleFunction ->
            "Package:Module:Function"

        ModuleFunction ->
            "Module:Function"

        Function ->
            "Function"
