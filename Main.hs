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

gameLoop :: Rogalik -> IO ()
gameLoop Rogalik {rogalikQuit = True} = return ()
gameLoop rogalik = do
  putStr "> "
  hFlush stdout
  line <- getLine
  rogalik' <-
    case line of
      "j" -> do
        let rogalik' = rogalikMove D rogalik
        putStrLn $ renderRogalik rogalik'
        return rogalik'
      "k" -> do
        let rogalik' = rogalikMove U rogalik
        putStrLn $ renderRogalik rogalik'
        return rogalik'
      "h" -> do
        let rogalik' = rogalikMove L rogalik
        putStrLn $ renderRogalik rogalik'
        return rogalik'
      "l" -> do
        let rogalik' = rogalikMove R rogalik
        putStrLn $ renderRogalik rogalik'
        return rogalik'
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
