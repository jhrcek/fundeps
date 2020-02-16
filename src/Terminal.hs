{-# LANGUAGE OverloadedStrings #-}

module Terminal
  ( Item (..),
    pickAnItem,
  )
where

import qualified Brick.AttrMap as Attr
import qualified Brick.Main as Brick
import Brick.Types (Widget)
import qualified Brick.Types as T
import Brick.Util (on)
import qualified Brick.Widgets.Border as Border
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core (hLimit, str, vLimit, withAttr)
import qualified Brick.Widgets.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Lens.Micro ((^.))

class Item a where
  showItem :: a -> String

type Model a = L.List () a

appDraw :: Item a => Model a -> [Widget ()]
appDraw l = [C.center box]
  where
    label = str "Multiple functions match your query. Pick one and press Enter"
    box =
      Border.borderWithLabel label
        $ hLimit 80
        $ vLimit 20
        $ L.renderList listDrawElement True l

appHandleEvent :: Item a => Model a -> T.BrickEvent () e -> T.EventM () (T.Next (Model a))
appHandleEvent l (T.VtyEvent e) =
  case e of
    V.EvKey V.KEnter [] ->
      case l ^. L.listSelectedL of
        Nothing -> Brick.continue l
        Just _ -> Brick.halt l
    V.EvKey V.KEsc [] -> Brick.halt l
    ev -> L.handleListEvent ev l >>= Brick.continue
appHandleEvent l _ = Brick.continue l

listDrawElement :: Item a => Bool -> a -> Widget ()
listDrawElement isSelected
  | isSelected = withAttr L.listSelectedAttr . str . showItem
  | otherwise = str . showItem

initList :: Item a => [a] -> Model a
initList items = L.list () (Vec.fromList items) 0

appAttrMap :: Attr.AttrMap
appAttrMap =
  Attr.attrMap
    V.defAttr
    [ (L.listAttr, V.white `on` V.blue),
      (L.listSelectedAttr, V.blue `on` V.white)
    ]

pickItemApp :: Item a => Brick.App (Model a) e ()
pickItemApp =
  Brick.App
    { Brick.appDraw = appDraw,
      Brick.appChooseCursor = Brick.showFirstCursor,
      Brick.appHandleEvent = appHandleEvent,
      Brick.appStartEvent = return,
      Brick.appAttrMap = const appAttrMap
    }

pickAnItem :: Item a => NE.NonEmpty a -> IO a
pickAnItem items = do
  mItem <- L.listSelectedElement <$> Brick.defaultMain pickItemApp (initList $ NE.toList items)
  case mItem of
    Nothing -> error "WTF?! selected item should never be nothing, since we're working with NonEmpty List!"
    Just (_, item) -> pure item
