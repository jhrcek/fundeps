{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module TUI.Settings.Editor (editSettings) where

import qualified Graphics.Vty as V
import qualified Settings as S

import Brick
    ( App (..)
    , AttrName
    , BrickEvent (..)
    , EventM
    , Next
    , Padding (..)
    , Widget
    , attrName
    , clickable
    , continue
    , customMain
    , hLimit
    , halt
    , padLeft
    , padTop
    , str
    , withAttr
    , (<=>)
    )
import Brick.AttrMap (AttrMap, attrMap)
import Brick.Focus (focusRingCursor)
import Brick.Forms
    ( Form
    , FormFieldState
    , checkboxCustomField
    , focusedFormInputAttr
    , formFocus
    , formState
    , handleFormEvent
    , invalidFormInputAttr
    , newForm
    , radioField
    , renderForm
    , (@@=)
    )
import Brick.Util (on)
import Brick.Widgets.Border (border, borderWithLabel)
import Brick.Widgets.Center (center)
import Brick.Widgets.Edit (editAttr, editFocusedAttr)
import Data.Declaration
    ( NodeFormat (Function, ModuleFunction, PackageModuleFunction)
    )
import Data.GraphViz.Attributes.Complete (RankDir (..))
import Data.GraphViz.Commands (GraphvizCommand (Circo, Dot, Neato, TwoPi))
import Data.Text (Text)
import Data.Void (Void)
import Graphics.Vty.Attributes (black, blue, defAttr, red, white, yellow)
import Lens.Micro (Lens')
import Settings (Settings)


type FormState = Form Settings Void Name


theMap :: AttrMap
theMap =
    attrMap
        defAttr
        [ (editAttr, white `on` blue)
        , (editFocusedAttr, black `on` yellow)
        , (invalidFormInputAttr, white `on` red)
        , (focusedFormInputAttr, black `on` yellow)
        , (title, V.withStyle V.defAttr V.bold)
        ]


title :: AttrName
title = attrName "title"


data Name
    = AllowMultiEdgesCheckBox
    | ClusterByModuleCheckBox
    | ClusterByPackageCheckBox
    | IncludeExternalPackagesCheckBox
    | TransitiveReductionCheckBox
    | DependencyModeCallersRadio
    | DependencyModeCalleesRadio
    | DependencyModeExactRadio
    | GvCommandDotRadio
    | GvCommandNeatoRadio
    | GvCommandTwopiRadio
    | GvCommandCircoRadio
    | NodeFormatPackageModuleFunctionRadio
    | NodeFormatModuleFunctionRadio
    | NodeFormatFunctionRadio
    | RankdirFromLeftRadio
    | RankdirFromRightRadio
    | RankdirFromTopRadio
    | RankdirFromBottomRadio
    | SaveButton
    deriving stock (Eq, Ord, Show)


mkForm :: Settings -> FormState
mkForm =
    let label s w =
            padTop (Pad 1)
                . hLimit 30
                $ withAttr title (str s) <=> w
     in newForm
            [ unicodeCheckbox S.allowMultiEdges AllowMultiEdgesCheckBox "Allow multi edges"
            , unicodeCheckbox S.includeExternalPackages IncludeExternalPackagesCheckBox "Include external packages"
            , unicodeCheckbox S.transitiveReduction TransitiveReductionCheckBox "Transitive reduction"
            , unicodeCheckbox S.clusterByModule ClusterByModuleCheckBox "Cluster by module"
            , unicodeCheckbox S.clusterByPackage ClusterByPackageCheckBox "Cluster by package"
            , label "Dependency mode"
                @@= radioField
                    S.dependencyMode
                    [ (S.Callees, DependencyModeCallersRadio, "Callees")
                    , (S.Callers, DependencyModeCalleesRadio, "Callers")
                    , (S.Exact, DependencyModeExactRadio, "Exact")
                    ]
            , label "Graphviz command"
                @@= radioField
                    S.graphvizCommand
                    [ (Dot, GvCommandDotRadio, "Dot")
                    , (Neato, GvCommandNeatoRadio, "Neato")
                    , (TwoPi, GvCommandTwopiRadio, "Twopi")
                    , (Circo, GvCommandCircoRadio, "Circo")
                    ]
            , label "Node format"
                @@= radioField
                    S.nodeFormat
                    [ (PackageModuleFunction, NodeFormatPackageModuleFunctionRadio, "Package:Module:Function")
                    , (ModuleFunction, NodeFormatModuleFunctionRadio, "Module:Function")
                    , (Function, NodeFormatFunctionRadio, "Function")
                    ]
            , label "Direction of edges"
                @@= radioField
                    S.rankDir
                    [ (FromLeft, RankdirFromLeftRadio, "Left to right")
                    , (FromRight, RankdirFromRightRadio, "Right to left")
                    , (FromTop, RankdirFromTopRadio, "Top to bottom")
                    , (FromBottom, RankdirFromBottomRadio, "Bottom to top")
                    ]
            ]


unicodeCheckbox :: Lens' s Bool -> Name -> Text -> s -> FormFieldState s Void Name
unicodeCheckbox = checkboxCustomField '[' '\10003' ']'


draw :: FormState -> [Widget Name]
draw f = [center form]
  where
    form =
        borderWithLabel (str "Settings") $
            renderForm f <=> padLeft (Pad 25) button
    button =
        clickable SaveButton
            . border
            $ str "Save"


app :: App FormState Void Name
app =
    App
        { appDraw = draw
        , appHandleEvent = handleEvent
        , appChooseCursor = focusRingCursor formFocus
        , appStartEvent = return
        , appAttrMap = const theMap
        }


handleEvent :: FormState -> BrickEvent Name Void -> EventM Name (Next FormState)
handleEvent s event = case event of
    MouseDown SaveButton _ _ _ -> halt s
    VtyEvent (V.EvKey V.KEnter _) -> halt s
    VtyEvent V.EvResize{} -> continue s
    _ -> handleFormEvent event s >>= continue


editSettings :: Settings -> IO Settings
editSettings settings = do
    let buildVty = do
            v <- V.mkVty =<< V.standardIOConfig
            V.setMode (V.outputIface v) V.Mouse True
            return v
        form0 = mkForm settings
    initialVty <- buildVty
    formState <$> customMain initialVty buildVty Nothing app form0
