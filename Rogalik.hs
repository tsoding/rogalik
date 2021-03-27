module Rogalik where

import Data.Array
import Data.Ix
import Data.List
import Data.Foldable
import qualified Data.Map as M
import Data.Functor.Identity

import Board
import Items
import StateT

data Dir
  = L
  | R
  | U
  | D
  deriving (Eq, Ord, Show)

dirV2 :: Dir -> Cell
dirV2 L = Cell 0 (-1)
dirV2 R = Cell 0 1
dirV2 U = Cell (-1) 0
dirV2 D = Cell 1 0

data Index a = Index Int deriving (Eq, Ord, Ix, Show)

data Room = Room
  { roomRect :: Rect
  , roomItems :: M.Map Cell Item
  } deriving (Show)

mkRoom :: Rect -> Room
mkRoom rect = Room {roomRect = rect, roomItems = M.empty}

addItem :: Cell -> Item -> Room -> Room
addItem cell item room = room {roomItems = M.insert cell item items}
  where
    items = roomItems room

displayRoom :: Monad m => Room -> StateT (Board Char) m ()
displayRoom room = do
  fillRect (roomRect room) '.'
  let roomPos = rectCell1 $ roomRect room
  for_ (M.toList $ roomItems room) $ \(itemPos, item) ->
    fillCell (roomPos ^+^ itemPos) (itemChar item)

data Place
  = PlaceRoom (Index Room)
  deriving (Show)

data Player = Player
  { playerPlace :: Place
  , playerPos :: Cell
  , playerGold :: Int
  , playerWeapons :: [Weapon]
  } deriving (Show)

updatePlayerPos :: Cell -> Player -> Player
updatePlayerPos pos player = player {playerPos = pos}

data FloorCell
  = Empty
  | RoomFloor
  | Wall
  | Door
  deriving (Show)

floorCellToChar :: FloorCell -> Char
floorCellToChar Empty = ' '
floorCellToChar RoomFloor = '.'
floorCellToChar Wall = '#'
floorCellToChar Door = '+'

data Rogalik = Rogalik
  { rogalikBoard :: Board FloorCell
  , rogalikPlayerPos :: Cell
  , rogalikQuit :: Bool
  } deriving (Show)

quitRogalik :: Monad m => StateT Rogalik m ()
quitRogalik = updateState (\rogalik -> rogalik {rogalikQuit = True})

generateRogalik :: Int -> Int -> Rogalik
generateRogalik width height =
  Rogalik
    { rogalikBoard = board
    , rogalikPlayerPos = fst $ bounds $ boardArray board
    , rogalikQuit = False
    }
  where
    board = mkBoard width height RoomFloor

rogalikMove :: Monad m => Dir -> StateT Rogalik m ()
rogalikMove dir = do
  playerPos <- rogalikPlayerPos <$> getState
  updateState (\rogalik -> rogalik {rogalikPlayerPos = playerPos ^+^ dirV2 dir})

renderRogalik :: Rogalik -> [String]
renderRogalik rogalik =
  boardToLists $
  runIdentity $
  execStateT (do fillCell (rogalikPlayerPos rogalik) '@') $
  floorCellToChar <$> rogalikBoard rogalik
