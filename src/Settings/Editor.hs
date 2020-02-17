{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}

module Settings.Editor where

import Brick
import Brick.AttrMap (AttrMap, attrMap)
import Brick.Focus
  ( focusRingCursor,
  )
import Brick.Forms
  ( (@@=),
    Form,
    FormFieldState,
    checkboxCustomField,
    focusedFormInputAttr,
    formFocus,
    formState,
    handleFormEvent,
    invalidFormInputAttr,
    newForm,
    radioField,
    renderForm,
  )
import Brick.Util (on)
import Brick.Widgets.Border (border, borderWithLabel)
import Brick.Widgets.Center (center)
import Brick.Widgets.Edit (editAttr, editFocusedAttr)
import Data.GraphViz.Commands (GraphvizCommand (Circo, Dot, Neato, TwoPi))
import Data.Text (Text)
import Data.Void (Void)
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes (black, black, blue, defAttr, red, white, yellow)
import Lens.Micro (Lens')
import Settings (Settings)
import qualified Settings as S

theMap :: AttrMap
theMap =
  attrMap
    defAttr
    [ (editAttr, white `on` blue),
      (editFocusedAttr, black `on` yellow),
      (invalidFormInputAttr, white `on` red),
      (focusedFormInputAttr, black `on` yellow),
      (title, V.withStyle V.defAttr V.bold)
    ]

title :: AttrName
title = attrName "title"

data Name
  = AllowMultiEdgesCheckBox
  | IncludeExternalPackagesCheckBox
  | TransitiveReductionCheckBox
  | DependencyModeCallersRadio
  | DependencyModeCalleesRadio
  | GvCommandDotRadio
  | GvCommandNeatoRadio
  | GvCommandTwopiRadio
  | GvCommandCircoRadio
  | NodeFormatFullRadio
  | NodeFormatWithoutPackageRadio
  | SaveButton
  deriving (Eq, Ord, Show)

mkForm :: Settings -> Form Settings Void Name
mkForm =
  let label s w =
        padTop (Pad 1)
          $ hLimit 30
          $ (withAttr title $ str s) <=> w
   in newForm
        [ unicodeCheckbox S.allowMultiEdges AllowMultiEdgesCheckBox "Allow multi edges",
          unicodeCheckbox S.includeExternalPackages IncludeExternalPackagesCheckBox "Include external packages",
          unicodeCheckbox S.transitiveReduction TransitiveReductionCheckBox "Transitive reduction",
          label "Dependency mode"
            @@= radioField
              S.dependencyMode
              [ (S.Callees, DependencyModeCallersRadio, "Callees"),
                (S.Callers, DependencyModeCalleesRadio, "Callers")
              ],
          label "Graphviz command"
            @@= radioField
              S.graphvizCommand
              [ (Dot, GvCommandDotRadio, "Dot"),
                (Neato, GvCommandNeatoRadio, "Neato"),
                (TwoPi, GvCommandTwopiRadio, "Twopi"),
                (Circo, GvCommandCircoRadio, "Circo")
              ],
          label "Node format"
            @@= radioField
              S.nodeFormat
              [ (S.Full, NodeFormatFullRadio, "Package:Module:Function"),
                (S.WithoutPackage, NodeFormatWithoutPackageRadio, "Module:Function")
              ]
        ]

unicodeCheckbox :: Lens' s Bool -> Name -> Text -> s -> FormFieldState s Void Name
unicodeCheckbox = checkboxCustomField '[' '\10003' ']'

draw :: Form Settings e Name -> [Widget Name]
draw f = [center form]
  where
    form =
      borderWithLabel (str "Settings") $
        renderForm f <=> padLeft (Pad 25) button
    button =
      clickable SaveButton
        $ border
        $ str "Save"

app :: App (Form Settings Void Name) Void Name
app =
  App
    { appDraw = draw,
      appHandleEvent = \s event -> case event of
        VtyEvent (V.EvResize {}) -> continue s
        MouseDown SaveButton _ _ _ -> halt s
        _ -> handleFormEvent event s >>= continue,
      appChooseCursor = focusRingCursor formFocus,
      appStartEvent = return,
      appAttrMap = const theMap
    }

editSettings :: Settings -> IO Settings
editSettings settings = do
  let buildVty = do
        v <- V.mkVty =<< V.standardIOConfig
        V.setMode (V.outputIface v) V.Mouse True
        return v
      form0 = mkForm settings
  initialVty <- buildVty
  formState <$> customMain initialVty buildVty Nothing app form0

main :: IO ()
main = editSettings S.defaultSettings >>= print
