module Rogalik where

import Data.Array
import Data.Ix
import Data.List
import Data.Foldable

import Display
import Room
import Items
import StateT

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

updatePlayerPos :: V2 -> Player -> Player
updatePlayerPos pos player = player {playerPos = pos}

data Rogalik = Rogalik
  { rogalikRooms :: Array (Index Room) Room
  , rogalikPlayer :: Player
  , rogalikQuit :: Bool
  } deriving (Show)

updateRogalikPlayer :: Player -> Rogalik -> Rogalik
updateRogalikPlayer player rogalik = rogalik {rogalikPlayer = player}

getRoom :: Monad m => Index Room -> StateT Rogalik m Room
getRoom index = do
  rooms <- rogalikRooms <$> getState
  return $ rooms ! index

quitRogalik :: Monad m => StateT Rogalik m ()
quitRogalik = updateState (\rogalik -> rogalik {rogalikQuit = True})

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
    , rogalikQuit = False
    }
  where
    roomsIndexRange = (Index 0, Index (roomsCount - 1))
    rooms =
      [ addItem (V2 3 3) (GoldItem 69) $ mkRoom $ Rect 1 1 10 5
      , mkRoom $ Rect 20 15 10 10
      , mkRoom $ Rect 1 20 5 7
      ]
    roomsCount = length rooms

clampRect :: Rect -> V2 -> V2
clampRect (Rect x y w h) (V2 x0 y0) =
  V2 (clamp x0 x (x + w - 1)) (clamp y0 y (y + h - 1))
  where
    clamp x l h = min (max x l) h

rogalikMove :: Monad m => Dir -> StateT Rogalik m ()
rogalikMove dir = do
  player <- rogalikPlayer <$> getState
  room <- getRoom (playerRoom player)
  let Rect _ _ w h = roomRect room
  updateState $
    updateRogalikPlayer $
    updatePlayerPos
      (clampRect (Rect 0 0 w h) (playerPos player ^+^ dirV2 dir))
      player

displayPlayer :: Monad m => Rogalik -> StateT Display m ()
displayPlayer rogalik = putPixel playerScreenPos playerPixel
  where
    player = rogalikPlayer rogalik
    rooms = rogalikRooms rogalik
    playerScreenPos = playerRoomPosition ^+^ playerPos player
    playerRoomPosition =
      let Rect x y _ _ = roomRect (rooms ! playerRoom player)
       in V2 x y
    playerPixel = '@'

displayRooms :: Monad m => Rogalik -> StateT Display m ()
displayRooms rogalik = for_ (elems $ rogalikRooms rogalik) $ displayRoom

displayRogalik :: Monad m => Rogalik -> StateT Display m ()
displayRogalik rogalik = do
  displayRooms rogalik
  displayPlayer rogalik
