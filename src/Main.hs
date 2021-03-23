module Main where

import ApplyMove
import Checkers.FrontEnd.Terminal (frontend)
import Checkers.FrontEnd.Types
import Checkers.Types
import Moves
import ABSearch

fakeEngine move state = state

main :: IO ()
main =
  frontend
    GameConfig
      { engine = apply_move,
        blackMove = Human,
        redMove = Ai red_ai,
        state = initialGameState 
      }