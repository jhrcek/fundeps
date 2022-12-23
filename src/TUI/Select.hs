{-# LANGUAGE OverloadedStrings #-}

module TUI.Select
    ( Item (..)
    , pickAnItem
    )
where

import Brick.AttrMap qualified as Attr
import Brick.Main qualified as Brick
import Brick.Types (Widget)
import Brick.Types qualified as T
import Brick.Util (on)
import Brick.Widgets.Border qualified as Border
import Brick.Widgets.Center qualified as C
import Brick.Widgets.Core (hLimit, str, vLimit, withAttr)
import Brick.Widgets.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Vector qualified as Vec
import Data.Void (Void)
import Graphics.Vty qualified as V
import Lens.Micro.Mtl (use)


class Item a where
    showItem :: a -> String


type Model a = L.List () a


appDraw :: Item a => Model a -> [Widget ()]
appDraw l = [C.center box]
  where
    label = str "Multiple functions match your query. Pick one and press Enter"
    box =
        Border.borderWithLabel label
            . hLimit 80
            . vLimit 20
            $ L.renderList listDrawElement True l


appHandleEvent :: T.BrickEvent () Void -> T.EventM () (Model a) ()
appHandleEvent (T.VtyEvent e) =
    case e of
        V.EvKey V.KEnter [] -> do
            s <- use L.listSelectedL
            case s of
                Nothing -> pure ()
                Just _ -> Brick.halt
        V.EvKey V.KEsc [] -> Brick.halt
        ev -> L.handleListEvent ev
appHandleEvent _ = pure ()


listDrawElement :: Item a => Bool -> a -> Widget ()
listDrawElement isSelected
    | isSelected = withAttr L.listSelectedAttr . str . showItem
    | otherwise = str . showItem


initList :: [a] -> Model a
initList items = L.list () (Vec.fromList items) 0


appAttrMap :: Attr.AttrMap
appAttrMap =
    Attr.attrMap
        V.defAttr
        [ (L.listAttr, V.white `on` V.blue)
        , (L.listSelectedAttr, V.blue `on` V.white)
        ]


pickItemApp :: Item a => Brick.App (Model a) Void ()
pickItemApp =
    Brick.App
        { Brick.appDraw = appDraw
        , Brick.appChooseCursor = Brick.showFirstCursor
        , Brick.appHandleEvent = appHandleEvent
        , Brick.appStartEvent = pure ()
        , Brick.appAttrMap = const appAttrMap
        }


pickAnItem :: Item a => NE.NonEmpty a -> IO a
pickAnItem items = do
    mItem <- L.listSelectedElement <$> Brick.defaultMain pickItemApp (initList $ NE.toList items)
    case mItem of
        Nothing -> error "WTF?! selected item should never be nothing, since we're working with NonEmpty List!"
        Just (_, item) -> pure item
