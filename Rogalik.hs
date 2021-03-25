module Rogalik where

import Data.Array
import Data.Ix
import Data.List

import Display
import Room
import Items

data Dir
  = L
  | R
  | U
  | D
  deriving (Eq, Ord, Show)

dirV2 :: Dir -> V2
dirV2 L = V2 (-1) 0
dirV2 R = V2 1 0
dirV2 U = V2 0 (-1)
dirV2 D = V2 0 1

data Index a = Index Int deriving (Eq, Ord, Ix, Show)

data Player = Player
  { playerRoom :: Index Room
  , playerPos :: V2
  , playerGold :: Int
  , playerWeapons :: [Weapon]
  } deriving (Show)

playerMove :: Dir -> Player -> Player
playerMove dir player = player {playerPos = pos ^+^ dirV2 dir}
  where
    pos = playerPos player

data Rogalik = Rogalik
  { rogalikRooms :: Array (Index Room) Room
  , rogalikPlayer :: Player
  } deriving (Show)

generateRogalik :: Rogalik
generateRogalik =
  Rogalik
    { rogalikRooms = array roomsIndexRange $ zip (range roomsIndexRange) rooms
    , rogalikPlayer =
        Player
          { playerRoom = Index 1
          , playerPos = V2 0 0
          , playerGold = 0
          , playerWeapons = []
          }
    }
  where
    roomsIndexRange = (Index 0, Index (roomsCount - 1))
    rooms =
      [ addItem (V2 3 3) (GoldItem 69) $ mkRoom $ Rect 0 0 10 5
      , mkRoom $ Rect 20 20 10 10
      , mkRoom $ Rect 0 20 5 7
      ]
    roomsCount = length rooms

rogalikMove :: Dir -> Rogalik -> Rogalik
rogalikMove dir rogalik = rogalik {rogalikPlayer = playerMove dir player}
  where
    player = rogalikPlayer rogalik

renderPlayer :: Rogalik -> Display -> Display
renderPlayer rogalik display = putPixel playerScreenPos playerPixel display
  where
    player = rogalikPlayer rogalik
    rooms = rogalikRooms rogalik
    playerScreenPos = playerRoomPosition ^+^ playerPos player
    playerRoomPosition =
      let Rect x y _ _ = roomRect (rooms ! playerRoom player)
       in V2 x y
    playerPixel = '@'

renderRooms :: Rogalik -> Display -> Display
renderRooms rogalik display =
  foldl' (\display room -> renderRoom room display) display $
  elems $ rogalikRooms rogalik

renderRogalik :: Rogalik -> Display -> Display
renderRogalik rogalik = renderPlayer rogalik . renderRooms rogalik
