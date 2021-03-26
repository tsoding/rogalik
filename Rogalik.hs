module Rogalik where

import Data.Array
import Data.Ix
import Data.List
import Data.Foldable
import qualified Data.Map as M

import Display
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

data Room = Room
  { roomRect :: Rect
  , roomItems :: M.Map V2 Item
  , roomNbors :: M.Map V2 Place
  } deriving Show

mkRoom :: Rect -> Room
mkRoom rect = Room {roomRect = rect, roomItems = M.empty, roomNbors = M.empty}

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
  fillRect (roomRect room) '.'
  displayRoomFrame room
  let roomPos = rectPos $ roomRect room
  for_ (M.toList $ roomItems room) $ \(itemPos, item) ->
    putPixel (roomPos ^+^ itemPos) (itemChar item)

data Place
  = PlaceRoom (Index Room)
  | PlacePassage (Index Passage)
  deriving (Show)

data Passage = Passage
  { passageLine :: Line
  , passageNbors :: M.Map V2 Place
  } deriving (Show)

data Player = Player
  { playerPlace :: Place
  , playerPos :: V2
  , playerGold :: Int
  , playerWeapons :: [Weapon]
  } deriving (Show)

updatePlayerPos :: V2 -> Player -> Player
updatePlayerPos pos player = player {playerPos = pos}

data Rogalik = Rogalik
  { rogalikRooms :: Array (Index Room) Room
  , rogalikPassages :: Array (Index Passage) Passage
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

clampPlayer :: Monad m => StateT Rogalik m ()
clampPlayer = do
  player <- rogalikPlayer <$> getState
  player' <-
    case playerPlace player of
      PlaceRoom index -> do
        room <- (! index) . rogalikRooms <$> getState
        return $
          updatePlayerPos (clampRect (roomRect room) (playerPos player)) player
      PlacePassage index -> do
        passage <- (! index) . rogalikPassages <$> getState
        return $
          updatePlayerPos
            (clampLine (passageLine passage) (playerPos player))
            player
  updateState (updateRogalikPlayer player')

generateRogalik :: Rogalik
generateRogalik =
  Rogalik
    { rogalikRooms = array roomsIndexRange $ zip (range roomsIndexRange) rooms
    , rogalikPassages =
        array passagesIndexRange $ zip (range passagesIndexRange) passages
    , rogalikPlayer =
        Player
          { playerPlace = PlaceRoom (Index 0)
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
    passagesIndexRange = (Index 0, Index (passagesCount - 1))
    passages = [Passage (Vert 5 7 15) M.empty]
    passagesCount = length passages

rogalikMove :: Monad m => Dir -> StateT Rogalik m ()
rogalikMove dir = do
  player <- rogalikPlayer <$> getState
  updateState $
    updateRogalikPlayer $
    updatePlayerPos (playerPos player ^+^ dirV2 dir) player
  clampPlayer

displayPassage :: Monad m => Passage -> StateT Display m ()
displayPassage passage = putLine (passageLine passage) '#'

displayRogalik :: Monad m => Rogalik -> StateT Display m ()
displayRogalik rogalik = do
  for_ (elems $ rogalikRooms rogalik) $ displayRoom
  for_ (elems $ rogalikPassages rogalik) $ displayPassage
  putPixel (playerPos $ rogalikPlayer rogalik) '@'
