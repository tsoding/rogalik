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

dirV2 :: Dir -> Point
dirV2 L = Point 0 (-1)
dirV2 R = Point 0 1
dirV2 U = Point (-1) 0
dirV2 D = Point 1 0

data Room = Room
  { roomRect :: Rect
  , roomItems :: M.Map Point Item
  } deriving (Show)

mkRoom :: Rect -> Room
mkRoom rect = Room {roomRect = rect, roomItems = M.empty}

addItem :: Point -> Item -> Room -> Room
addItem cell item room = room {roomItems = M.insert cell item items}
  where
    items = roomItems room

displayRoom :: Monad m => Room -> StateT (Board Char) m ()
displayRoom room = do
  modify $ fillRect (roomRect room) '.'
  let roomPos = rectPoint1 $ roomRect room
  for_ (M.toList $ roomItems room) $ \(itemPos, item) ->
    modify $ fillPoint (roomPos ^+^ itemPos) (itemChar item)

data Player = Player
  { playerPos :: Point
  , playerGold :: Int
  , playerWeapons :: [Weapon]
  } deriving (Show)

data Cell
  = Empty
  | Floor
  | VertWall
  | HorzWall
  | Passage
  | Door
  deriving (Show)

cellToChar :: Cell -> Char
cellToChar Empty = ' '
cellToChar Floor = '.'
cellToChar VertWall = '|'
cellToChar HorzWall = '-'
cellToChar Passage = '#'
cellToChar Door = '+'

cellWalkable :: Cell -> Bool
cellWalkable Empty = False
cellWalkable Floor = True
cellWalkable VertWall = False
cellWalkable HorzWall = False
cellWalkable Passage = True
cellWalkable Door = True

data Rogalik = Rogalik
  { rogalikBoard :: Board Cell
  , rogalikPlayerPos :: Point
  , rogalikQuit :: Bool
  } deriving (Show)

rogalikUpdateBoard :: Monad m => StateT (Board Cell) m () -> StateT Rogalik m ()
rogalikUpdateBoard boardState = StateT $ \rogalik -> do
  board' <- execStateT boardState $ rogalikBoard rogalik
  return ((), rogalik { rogalikBoard = board' })

quitRogalik :: Monad m => StateT Rogalik m ()
quitRogalik = modify (\rogalik -> rogalik {rogalikQuit = True})

emptyRogalik :: Int -> Int -> Rogalik
emptyRogalik width height =
  Rogalik
    { rogalikBoard = board
    , rogalikPlayerPos = (fst $ bounds $ boardArray board) ^+^ Point 1 1
    , rogalikQuit = False
    }
  where
    board = mkBoard width height Empty

generateRoomRect :: Monad m => Rect -> StateT Rogalik m ()
generateRoomRect rect@(Rect (Point row1 col1) (Point row2 col2)) = rogalikUpdateBoard $ do
  modify $ fillRect (Rect (Point row1 col1) (Point row2 col1)) VertWall
  modify $ fillRect (Rect (Point row1 col2) (Point row2 col2)) VertWall
  modify $ fillRect (Rect (Point row1 col1) (Point row1 col2)) HorzWall
  modify $ fillRect (Rect (Point row2 col1) (Point row2 col2)) HorzWall
  modify $ fillRect (shrinkRect 1 rect) Floor

generateRoomAt :: Monad m => Point -> Int -> Int -> StateT Rogalik m ()
generateRoomAt pos rows cols = generateRoomRect (Rect pos (pos ^+^ Point (rows - 1) (cols - 1)))

randomPoint :: Rect -> IO Point
randomPoint (Rect (Point row1 col1) (Point row2 col2)) = do
  row <- randomRIO (row1, row2)
  col <- randomRIO (col1, col2)
  return $ Point row col

generateRooms :: StateT Rogalik IO ()
generateRooms =
  replicateM_ 2 $ do
    board <- rogalikBoard <$> get
    cell <- lift $ randomPoint $ boardRect board
    w <- lift $ randomRIO (3, 7)
    h <- lift $ randomRIO (3, 7)
    generateRoomAt cell w h

placePlayer :: StateT Rogalik IO ()
placePlayer = do
  board <- rogalikBoard <$> get
  let walkables = filter (cellWalkable . snd) $ assocs $ boardArray board
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
  let playerPos' = rogalikPlayerPos rogalik ^+^ dirV2 dir
      board = rogalikBoard rogalik
   in if cellWalkable $ board ^!^ playerPos'
      then rogalik {rogalikPlayerPos = playerPos'}
      else rogalik

renderRogalik :: Rogalik -> [String]
renderRogalik rogalik =
  boardToLists $
  runIdentity $
  execStateT (do modify $ fillPoint (rogalikPlayerPos rogalik) '@') $
  cellToChar <$> rogalikBoard rogalik
