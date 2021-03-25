module Room where

import Display
import Items
import qualified Data.Map as M
import Data.List

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

renderRoom :: Room -> Display -> Display
renderRoom room display =
  foldl'
    (\display ((V2 itemX itemY), item) ->
       putPixel (V2 (itemX + roomX) (itemY + roomY)) (itemChar item) display)
    (fillRect (roomRect room) roomFloor display) $
  M.toList $ roomItems room
  where
    Rect roomX roomY _ _ = roomRect room

testRoom :: Room
testRoom =
  addItem (V2 1 1) (WeaponItem Sword) $
  addItem (V2 0 0) (GoldItem 69) $ mkRoom $ Rect 0 0 10 10
