module Heuristic where

import Checkers.Types

-- | The 'blackHeuristic' function calculates a value given a gamestate from the perspective of Black
blackHeuristic:: GameState -> Int
blackHeuristic st = length (blackPieces st) - length (redPieces st) + 2 * (length (blackKings st) - length (redKings st))

-- | The 'redHeuristic' function calculates a value given a gamestate from the perspective of Red
redHeuristic:: GameState -> Int
redHeuristic st = length (redPieces st) - length (blackPieces st) + 2 * (length (redKings st) - length (blackKings st))