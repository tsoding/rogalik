{-# LANGUAGE DeriveFunctor #-}
module Board where

import StateT

import Data.Ix
import Data.Array
import Data.List
import Data.Function

data Cell = Cell
  { cellRow :: Int
  , cellCol :: Int
  } deriving (Eq, Ord, Ix, Show)

(^+^) :: Cell -> Cell -> Cell
(^+^) (Cell row1 col1) (Cell row2 col2) = Cell (row1 + row2) (col1 + col2)

(^-^) :: Cell -> Cell -> Cell
(^-^) (Cell row1 col1) (Cell row2 col2) = Cell (row1 - row2) (col1 - col2)

data Rect = Rect
  { rectCell1 :: Cell
  , rectCell2 :: Cell
  } deriving (Show)

clampRect :: Rect -> Cell -> Cell
clampRect (Rect (Cell row1 col1) (Cell row2 col2)) (Cell row col) =
  Cell (clamp row row1 row2) (clamp col col1 col2)
  where
    clamp x l h = min (max x l) h

data Board a = Board
  { boardArray :: Array Cell a
  } deriving (Show, Functor)

mkBoard :: Int -> Int -> a -> Board a
mkBoard width height a =
  Board $ array cellRange $ zip (range cellRange) (cycle [a])
  where
    cellRange = (Cell 1 1, Cell height width)

boardToLists :: Board a -> [[a]]
boardToLists (Board pixels) =
  map (map snd) $ groupBy ((==) `on` (cellRow . fst)) $ assocs pixels

fillCell :: Monad m => Cell -> a -> StateT (Board a) m ()
fillCell cell = fillRect (Rect cell cell)

fillRect :: Monad m => Rect -> a -> StateT (Board a) m ()
fillRect (Rect cell1 cell2) a =
  StateT $ \board ->
    let pixels = boardArray board
     in return
          ( ()
          , board
              {boardArray = pixels // zip (range (cell1, cell2)) (cycle [a])})

fillBoard :: Monad m => a -> StateT (Board a) m ()
fillBoard a = do
  StateT $ \board ->
    let pixels = boardArray board
     in return
          ((), board {boardArray = pixels // zip (indices pixels) (cycle [a])})