module Rogalik where

import Data.Array
import Data.Foldable
import qualified Data.Map as M
import Data.Functor.Identity
import System.Random
import Control.Monad

import Board
import Items
import Control.Monad.Trans.State
import Control.Monad.Trans.Class

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
  modify $ fillRect (roomRect room) '.'
  let roomPos = rectCell1 $ roomRect room
  for_ (M.toList $ roomItems room) $ \(itemPos, item) ->
    modify $ fillCell (roomPos ^+^ itemPos) (itemChar item)

data Player = Player
  { playerPos :: Cell
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
  modify $ fillRect (Rect (Cell row1 col1) (Cell row2 col1)) VertWall
  modify $ fillRect (Rect (Cell row1 col2) (Cell row2 col2)) VertWall
  modify $ fillRect (Rect (Cell row1 col1) (Cell row1 col2)) HorzWall
  modify $ fillRect (Rect (Cell row2 col1) (Cell row2 col2)) HorzWall
  modify $ fillRect (shrinkRect 1 rect) RoomFloor

generateRoomAt :: Monad m => Cell -> Int -> Int -> StateT Rogalik m ()
generateRoomAt pos rows cols = generateRoomRect (Rect pos (pos ^+^ Cell (rows - 1) (cols - 1)))

randomCell :: Rect -> IO Cell
randomCell (Rect (Cell row1 col1) (Cell row2 col2)) = do
  row <- randomRIO (row1, row2)
  col <- randomRIO (col1, col2)
  return $ Cell row col

generateRooms :: StateT Rogalik IO ()
generateRooms =
  replicateM_ 2 $ do
    board <- rogalikBoard <$> get
    cell <- lift $ randomCell $ boardRect board
    w <- lift $ randomRIO (3, 7)
    h <- lift $ randomRIO (3, 7)
    generateRoomAt cell w h

placePlayer :: StateT Rogalik IO ()
placePlayer = do
  board <- rogalikBoard <$> get
  let walkables = filter (floorCellWalkable . snd) $ assocs $ boardArray board
  let n = length walkables
  when (n == 0) $ error "Could not find any walkable cells for the player"
  i <- lift $ randomRIO (0, n - 1)
  modify $ \rogalik -> rogalik {rogalikPlayerPos = fst (walkables !! i)}

generateRogalik :: StateT Rogalik IO ()
generateRogalik = do
  generateRooms
  placePlayer

rogalikMove :: Monad m => Dir -> StateT Rogalik m ()
rogalikMove dir = modify $ \rogalik ->
  let playerPos' = rogalikPlayerPos rogalik^+^ dirV2 dir
      board = rogalikBoard rogalik
   in if floorCellWalkable $ board ^!^ playerPos'
      then rogalik {rogalikPlayerPos = playerPos'}
      else rogalik

renderRogalik :: Rogalik -> [String]
renderRogalik rogalik =
  boardToLists $
  runIdentity $
  execStateT (do modify $ fillCell (rogalikPlayerPos rogalik) '@') $
  floorCellToChar <$> rogalikBoard rogalik
