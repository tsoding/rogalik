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

data Rect =
  Rect Int
       Int
       Int
       Int
  deriving (Show)

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

putPixel :: V2 -> Pixel -> StateT Display ()
putPixel (V2 x y) = fillRect $ Rect x y 1 1

putVertLine :: Int -> Int -> Int -> Pixel -> StateT Display ()
putVertLine x y1 y2 = fillRect $ Rect x y1 1 (y2 - y1 + 1)

putHorLine :: Int -> Int -> Int -> Pixel -> StateT Display ()
putHorLine y x1 x2 = fillRect $ Rect x1 y (x2 - x1 + 1) 1

fillRect :: Rect -> Pixel -> StateT Display ()
fillRect (Rect x y w h) pixel =
  StateT $ \display ->
    let V2 width height = displaySize display
        pixels = displayPixels display
     in ( display
            { displayPixels =
                pixels // do
                  x <- [x .. (x + w - 1)]
                  y <- [y .. (y + h - 1)]
                  return (V2 (x `mod` width) (y `mod` height), pixel)
            }
        , ())

fillDisplay :: Pixel -> StateT Display ()
fillDisplay pixel = do
  V2 width height <- displaySize <$> getState
  fillRect (Rect 0 0 width height) pixel
