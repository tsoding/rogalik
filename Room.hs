module Room where

import Display
import Items
import StateT

import qualified Data.Map as M
import Data.List
import Data.Foldable

data Room = Room
  { roomRect :: Rect
  , roomItems :: M.Map V2 Item
  } deriving Show

roomFloor :: Pixel
roomFloor = '.'

mkRoom :: Rect -> Room
mkRoom rect = Room rect M.empty

addItem :: V2 -> Item -> Room -> Room
addItem cell item room = room {roomItems = M.insert cell item items}
  where
    items = roomItems room

displayRoomFrame :: Monad m => Room -> StateT Display m ()
displayRoomFrame room = do
  let rect@(Rect x y w h) = roomRect room
  -- Corners
  putPixel (V2 (x - 1) (y - 1)) '+'
  putPixel (V2 (x + w) (y - 1)) '+'
  putPixel (V2 (x - 1) (y + h)) '+'
  putPixel (V2 (x + w) (y + h)) '+'
  -- Sides
  putLine (Horz (y - 1) x (x + w - 1)) '-'
  putLine (Horz (y + h) x (x + w - 1)) '-'
  putLine (Vert (x - 1) y (y + h - 1)) '|'
  putLine (Vert (x + w) y (y + h - 1)) '|'

displayRoom :: Monad m => Room -> StateT Display m ()
displayRoom room = do
  fillRect (roomRect room) roomFloor
  displayRoomFrame room
  let roomPos = rectPos $ roomRect room
  for_ (M.toList $ roomItems room) $ \(itemPos, item) ->
    putPixel (roomPos ^+^ itemPos) (itemChar item)
    
testRoom :: Room
testRoom =
  addItem (V2 1 1) (WeaponItem Sword) $
  addItem (V2 0 0) (GoldItem 69) $ mkRoom $ Rect 0 0 10 10
