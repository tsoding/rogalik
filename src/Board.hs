{-# LANGUAGE DeriveFunctor #-}
module Board where

import Data.Ix
import Data.Array
import Data.List
import Data.Function

data Point = Point
  { cellRow :: Int
  , cellCol :: Int
  } deriving (Eq, Ord, Ix, Show)

liftPoint2 :: (Int -> Int -> Int) -> Point -> Point -> Point
liftPoint2 op (Point row1 col1) (Point row2 col2) = Point (row1 `op` row2) (col1 `op` col2)

(^+^) :: Point -> Point -> Point
(^+^) = liftPoint2 (+)

(^-^) :: Point -> Point -> Point
(^-^) = liftPoint2 (-)

data Rect = Rect
  { rectPoint1 :: Point
  , rectPoint2 :: Point
  } deriving (Show)

boardRect :: Board a -> Rect
boardRect board = uncurry Rect $ bounds $ boardArray board

shrinkRect :: Int -> Rect -> Rect
shrinkRect s (Rect cell1 cell2) = Rect (cell1 ^+^ Point s s) (cell2 ^-^ Point s s)

clampRect :: Rect -> Point -> Point
clampRect (Rect (Point row1 col1) (Point row2 col2)) (Point row col) =
  Point (clamp row row1 row2) (clamp col col1 col2)
  where
    clamp x l h = min (max x l) h

data Board a = Board
  { boardArray :: Array Point a
  } deriving (Show, Functor)

(^!^) :: Board a -> Point -> a
(^!^) board cell = boardArray board ! wrapPoint board cell

wrapPoint :: Board a -> Point -> Point
wrapPoint board cell = liftPoint2 mod (cell ^-^ offset) size ^+^ offset
  where (offset, t) = bounds $ boardArray board
        size = t ^-^ offset

mkBoard :: Int -> Int -> a -> Board a
mkBoard width height a =
  Board $ array cellRange $ zip (range cellRange) (cycle [a])
  where
    cellRange = (Point 1 1, Point height width)

boardToLists :: Board a -> [[a]]
boardToLists (Board pixels) =
  map (map snd) $ groupBy ((==) `on` (cellRow . fst)) $ assocs pixels

fillPoint :: Point -> a -> Board a -> Board a
fillPoint cell = fillRect (Rect cell cell)

fillRect :: Rect -> a -> Board a -> Board a
fillRect (Rect cell1 cell2) a board = board {boardArray = pixels // patch}
  where pixels = boardArray board
        patch = zip (map (wrapPoint board) $ range (cell1, cell2)) (cycle [a])

fillBoard :: a -> Board a -> Board a
fillBoard a board = fillRect (boardRect board) a board
