module Main where

import Text.Printf
import System.IO
import Control.Monad
import Data.Functor

import Rogalik
import Display
import StateT
    
-- TODO: random level generation based on RNG
-- TODO: character movement bounding
-- TODO: passages between the rooms?
-- TODO: enemies
-- TODO: floors
-- TODO: potions
-- ...

renderRogalik :: StateT Rogalik IO ()
renderRogalik = do
  rogalik <- getState
  (_, display) <- runStateT (displayRogalik rogalik) stdDisplay
  lift $ putStrLn $ renderDisplay display

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
        renderRogalik
      "k" -> do
        rogalikMove U
        renderRogalik
      "h" -> do
        rogalikMove L
        renderRogalik
      "l" -> do
        rogalikMove R
        renderRogalik
      "q" -> quitRogalik
      "help" -> lift $ printf "Use vim keybindings to navigate loooool\n"
      _ -> lift $printf "Unknown command: %s\n" line
    gameLoop

main :: IO ()
main = void $ runStateT (renderRogalik >> gameLoop) generateRogalik
