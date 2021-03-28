module Rogalik where

import Data.Array
import Data.Ix
import Data.List
import Data.Foldable
import qualified Data.Map as M
import Data.Functor.Identity

import Board
import Items
import Control.Monad.Trans.State

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

data FloorCell
  = Empty
  | RoomFloor
  | VertWall
  | HorzWall
  | Passage
  | Door
  deriving (Show)

floorCellToChar :: FloorCell -> Char
floorCellToChar Empty = ' '
floorCellToChar RoomFloor = '.'
floorCellToChar VertWall = '|'
floorCellToChar HorzWall = '-'
floorCellToChar Passage = '#'
floorCellToChar Door = '+'

floorCellWalkable :: FloorCell -> Bool
floorCellWalkable Empty = False
floorCellWalkable RoomFloor = True
floorCellWalkable VertWall = False
floorCellWalkable HorzWall = False
floorCellWalkable Passage = True
floorCellWalkable Door = True

data Rogalik = Rogalik
  { rogalikBoard :: Board FloorCell
  , rogalikPlayerPos :: Cell
  , rogalikQuit :: Bool
  } deriving (Show)

rogalikUpdateBoard :: Monad m => StateT (Board FloorCell) m () -> StateT Rogalik m ()
rogalikUpdateBoard boardState = StateT $ \rogalik -> do
  board' <- execStateT boardState $ rogalikBoard rogalik
  return ((), rogalik { rogalikBoard = board' })

quitRogalik :: Monad m => StateT Rogalik m ()
quitRogalik = modify (\rogalik -> rogalik {rogalikQuit = True})

emptyRogalik :: Int -> Int -> Rogalik
emptyRogalik width height =
  Rogalik
    { rogalikBoard = board
    , rogalikPlayerPos = (fst $ bounds $ boardArray board) ^+^ Cell 1 1
    , rogalikQuit = False
    }
  where
    board = mkBoard width height Empty

generateRoomRect :: Monad m => Rect -> StateT Rogalik m ()
generateRoomRect rect@(Rect (Cell row1 col1) (Cell row2 col2)) = rogalikUpdateBoard $ do
  fillRect (Rect (Cell row1 col1) (Cell row2 col1)) VertWall
  fillRect (Rect (Cell row1 col2) (Cell row2 col2)) VertWall
  fillRect (Rect (Cell row1 col1) (Cell row1 col2)) HorzWall
  fillRect (Rect (Cell row2 col1) (Cell row2 col2)) HorzWall
  fillRect (shrinkRect 1 rect) RoomFloor

generateRooms :: Monad m => StateT Rogalik m ()
generateRooms = do
  generateRoomRect (Rect (Cell 1 1) (Cell 5 5))

generateRogalik :: Monad m => StateT Rogalik m ()
generateRogalik = do
  generateRooms

rogalikMove :: Monad m => Dir -> StateT Rogalik m ()
rogalikMove dir = modify $ \rogalik ->
  let playerPos = rogalikPlayerPos rogalik
      playerPos' = playerPos ^+^ dirV2 dir
      board = rogalikBoard rogalik
   in if floorCellWalkable $ board ^!^ playerPos'
      then rogalik {rogalikPlayerPos = playerPos'}
      else rogalik

renderRogalik :: Rogalik -> [String]
renderRogalik rogalik =
  boardToLists $
  runIdentity $
  execStateT (do fillCell (rogalikPlayerPos rogalik) '@') $
  floorCellToChar <$> rogalikBoard rogalik
