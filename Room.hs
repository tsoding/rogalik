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

displayRoom :: Room -> StateT Display ()
displayRoom room = do
  fillRect (roomRect room) roomFloor
  let roomPos = rectPos $ roomRect room
  for_ (M.toList $ roomItems room) $ \(itemPos, item) ->
    putPixel (roomPos ^+^ itemPos) (itemChar item)
    
testRoom :: Room
testRoom =
  addItem (V2 1 1) (WeaponItem Sword) $
  addItem (V2 0 0) (GoldItem 69) $ mkRoom $ Rect 0 0 10 10
