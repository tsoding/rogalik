module Main where

import Text.Printf
import System.IO
import Control.Monad

import Rogalik
import Display

-- TODO: random level generation based on RNG
-- TODO: frames around the rooms?
-- TODO: passages between the rooms?
-- TODO: enemies
-- TODO: floors
-- TODO: potions
-- ...

renderRogalik :: Rogalik -> String
renderRogalik rogalik =
  renderDisplay $ displayRogalik rogalik stdDisplay

processMoveKey :: Rogalik -> Dir -> IO Rogalik
processMoveKey rogalik dir = do
    let rogalik' = rogalikMove dir rogalik
    putStrLn $ renderRogalik rogalik'
    return rogalik'

gameLoop :: Rogalik -> IO ()
gameLoop Rogalik {rogalikQuit = True} = return ()
gameLoop rogalik = do
  putStr "> "
  hFlush stdout
  line <- getLine
  rogalik' <-
    case line of
      "j" -> do
        processMoveKey rogalik D
      "k" -> processMoveKey rogalik U
      "h" -> processMoveKey rogalik L
      "l" -> processMoveKey rogalik R
      "q" -> return $ quitRogalik rogalik
      "help" -> do
        printf "Use vim keybindings to navigate loooool\n"
        return rogalik
      _ -> do
        printf "Unknown command: %s\n" line
        return rogalik
  gameLoop rogalik'

main :: IO ()
main = do
  let rogalik = generateRogalik
  putStrLn $ renderRogalik rogalik
  gameLoop rogalik
