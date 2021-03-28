module Main where

import Text.Printf
import System.IO
import Control.Monad
import Data.Functor
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Data.Foldable

import Rogalik
import Board

-- TODO: random level generation based on RNG
-- TODO: enemies
-- TODO: floors
-- TODO: potions
-- ...

printRogalik :: StateT Rogalik IO ()
printRogalik = do
  rogalik <- get
  lift $ putStrLn $ unlines $ renderRogalik rogalik

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM conditionM body = do
  condition <- conditionM
  unless condition body

handleCommands :: String -> StateT Rogalik IO ()
handleCommands commands =
  for_ commands $ \command ->
    case command of
      's' -> rogalikMove D
      'w' -> rogalikMove U
      'a' -> rogalikMove L
      'd' -> rogalikMove R
      'q' -> quitRogalik
      _ -> lift $ printf "Unknown command: %c\n" command

gameLoop :: StateT Rogalik IO ()
gameLoop =
  unlessM (rogalikQuit <$> get) $ do
    lift $ putStr "> "
    lift $ hFlush stdout
    line <- lift $ getLine
    handleCommands line
    printRogalik
    gameLoop

main :: IO ()
main =
  evalStateT
    (do generateRogalik
        printRogalik
        gameLoop) $
  emptyRogalik 20 20
