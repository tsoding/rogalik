module Display where

import StateT

import Data.Ix
import Data.Array

type Pixel = Char
data V2 = V2 Int Int deriving (Eq, Ord, Ix, Show)

(^+^) :: V2 -> V2 -> V2
(^+^) (V2 x1 y1) (V2 x2 y2) = V2 (x1 + x2) (y1 + y2)

(^-^) :: V2 -> V2 -> V2
(^-^) (V2 x1 y1) (V2 x2 y2) = V2 (x1 - x2) (y1 - y2)

data Rect = Rect
  { rectX :: Int
  , rectY :: Int
  , rectW :: Int
  , rectH :: Int
  } deriving (Show)

clampRect :: Rect -> V2 -> V2
clampRect (Rect x y w h) (V2 x0 y0) =
  V2 (clamp x0 x (x + w - 1)) (clamp y0 y (y + h - 1))
  where
    clamp x l h = min (max x l) h

data Line
  = Vert { vertX :: Int
         , vertY1 :: Int
         , vertY2 :: Int }
  | Horz { horzY :: Int
         , horzX1 :: Int
         , horzX2 :: Int }
  deriving (Show)

clampLine :: Line -> V2 -> V2
clampLine (Vert x y1 y2) = clampRect (Rect x x y1 y2)
clampLine (Horz y x1 x2) = clampRect (Rect x1 x2 y y)

rectPos :: Rect -> V2
rectPos (Rect x y _ _) = V2 x y

data Display = Display
  { displaySize :: V2
  , displayPixels :: Array V2 Pixel
  } deriving Show

stdDisplay :: Display
stdDisplay = mkDisplay (V2 80 30) ' '
  where
    mkDisplay :: V2 -> Pixel -> Display
    mkDisplay size@(V2 width height) pixel = Display size pixels
      where
        cellRange = (V2 0 0, V2 (width - 1) (height - 1))
        pixels =
          array cellRange $ do
            cell <- range cellRange
            return (cell, pixel)

renderDisplay :: Display -> String
renderDisplay display =
  unlines [[pixels ! V2 x y | x <- [0 .. width - 1]] | y <- [0 .. height - 1]]
  where
    V2 width height = displaySize display
    pixels = displayPixels display

putPixel :: Monad m => V2 -> Pixel -> StateT Display m ()
putPixel (V2 x y) = fillRect $ Rect x y 1 1

putLine :: Monad m => Line -> Pixel -> StateT Display m ()
putLine (Vert x y1 y2) = fillRect $ Rect x y1 1 (y2 - y1 + 1)
putLine (Horz y x1 x2) = fillRect $ Rect x1 y (x2 - x1 + 1) 1

fillRect :: Monad m => Rect -> Pixel -> StateT Display m ()
fillRect (Rect x y w h) pixel =
  StateT $ \display ->
    let V2 width height = displaySize display
        pixels = displayPixels display
     in return
          ( ()
          , display
              { displayPixels =
                  pixels // do
                    x <- [x .. (x + w - 1)]
                    y <- [y .. (y + h - 1)]
                    return (V2 (x `mod` width) (y `mod` height), pixel)
              })

fillDisplay :: Monad m => Pixel -> StateT Display m ()
fillDisplay pixel = do
  V2 width height <- displaySize <$> getState
  fillRect (Rect 0 0 width height) pixel
