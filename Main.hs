module Main where

import Text.Printf
import System.IO
import Control.Monad
import Data.Functor

import Rogalik
import Board
import StateT

-- TODO: random level generation based on RNG
-- TODO: passages between the rooms?
-- TODO: enemies
-- TODO: floors
-- TODO: potions
-- ...

printRogalik :: StateT Rogalik IO ()
printRogalik = do
  rogalik <- getState
  lift $ putStrLn $ unlines $ renderRogalik rogalik

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM conditionM body = do
  condition <- conditionM
  unless condition body

gameLoop :: StateT Rogalik IO ()
gameLoop =
  unlessM (rogalikQuit <$> getState) $ do
    lift $ putStr "> "
    lift $ hFlush stdout
    line <- lift $ getLine
    case line of
      "j" -> do
        rogalikMove D
        printRogalik
      "k" -> do
        rogalikMove U
        printRogalik
      "h" -> do
        rogalikMove L
        printRogalik
      "l" -> do
        rogalikMove R
        printRogalik
      "q" -> quitRogalik
      "help" -> lift $ printf "Use vim keybindings to navigate loooool\n"
      _ -> lift $ printf "Unknown command: %s\n" line
    gameLoop

main :: IO ()
main =
  evalStateT
    (do generateRogalik
        printRogalik
        gameLoop) $
  emptyRogalik 10 10
