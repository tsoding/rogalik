module Main where

import Rogalik
import Display

main :: IO ()
main = do
  let display = mkDisplay (V2 80 30) ' '
  let rogalik = generateRogalik
  putStrLn $ renderDisplay $ renderRogalik rogalik display
