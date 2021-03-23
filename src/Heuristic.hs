module Heuristic where

import Checkers.Types

-- | The 'blackHeuristic' function calculates a value given a gamestate from the perspective of Black
blackHeuristic:: GameState -> Int
blackHeuristic st 
    | blackEndGame = sumOfDistances
    | blackMidGame = evaluateBlackPieces (blackPieces st) - evaluateRedPieces (redPieces st) + 2 * (evaluateKings (blackKings st) - evaluateKings (redKings st))
    | blackOpeningGame = length (blackPieces st) - length (redPieces st) + 2 * (length (blackKings st) - length (redKings st))
    where 
        -- Only kings remain
        blackEndGame = null (blackPieces st) && not (null (blackKings st)) 
        -- There are less than 7 pieces, could contain kings
        blackMidGame =  length (blackPieces st) < 7
        -- There are greater than or equal to 7 pieces, could contain kings (unlikely)
        blackOpeningGame = length (blackPieces st) >= 7
        -- For each king, calculate the sum of distance values between opponents kings and sum it all up
        -- When black has more kings, it is in attack mode so it tries to minimize the overall distance, hence distance is subtracted from 14 so that greater distance leads to lower distance value
        -- When red has more kings, black is in runAway mode so it tried to maximize the overall distance, the greater the distance, the greater the distance value  
        sumOfDistances
            | length (blackKings st) >= length (redKings st) = attack (blackKings st) (redKings st)
            | otherwise = runAway (blackKings st) (redKings st)

-- | The 'evaluateBlackPieces' function evaluates a score for each black piece based on row position (row 7 is the best)
evaluateBlackPieces :: [Coord] -> Int
evaluateBlackPieces = foldr (\(x,y) acc -> 5 + y + acc) 0

-- | The 'evaluateRedPieces' function evaluates a score for each red piece based on row position (row 0 is the best)
evaluateRedPieces ::  [Coord] -> Int
evaluateRedPieces = foldr (\(x,y) acc -> 5 + (7 - y) + acc) 0

-- | The 'evaluateKings' function evaluates a consistent score for each king (adds number of rows and 2 for being a king)
evaluateKings :: [Coord] -> Int
evaluateKings = foldr (\(x,y) acc -> 5 + 7 + 2 + acc) 0

-- | The 'attack' function calculates a value using the distance between each king and opponents kings using an attacking mindset (minimize distance)
attack :: (Foldable t1, Foldable t2, Num b) => t1 (b, b) -> t2 (b, b) -> b
attack attackers runners = foldr (\(x,y) acc -> foldr (\(a,b) sum -> 14 - (abs (x - a) + abs (y - b)) + sum ) 0 runners) 0 attackers

-- | The 'runAway' function calculates a value using the distance between each king and opponents kings using a running away mindset (maximize distance)
runAway :: (Foldable t1, Foldable t2, Num b) => t1 (b, b) -> t2 (b, b) -> b
runAway runners attackers = foldr (\(x,y) acc -> foldr (\(a,b) sum -> (abs (x - a) + abs (y - b)) + sum ) 0 attackers) 0 runners

-- | The 'redHeuristic' function calculates a value given a gamestate from the perspective of Red
redHeuristic:: GameState -> Int
redHeuristic st
    | redEndGame = sumOfDistances
    | redMidGame =  evaluateRedPieces (redPieces st) - evaluateBlackPieces (blackPieces st) + 2 * (evaluateKings (redKings st) - evaluateKings (blackKings st))
    | redOpeningGame = length (redPieces st) - length (blackPieces st) + 2 * (length (redKings st) - length (blackKings st))
    where 
        -- Only kings remain
        redEndGame = null (redPieces st) && not (null (redKings st)) 
        -- There are less than 7 pieces, could contain kings
        redMidGame =  length (redPieces st) < 7
        -- There are greater than or equal to 7 pieces, could contain kings (unlikely)
        redOpeningGame = length (redPieces st) >= 7
        -- For each king, calculate the sum of distance values between opponents kings and sum it all up
        -- When red has more kings, it is in attack mode so it tries to minimize the overall distance, hence distance is subtracted from 14 so that greater distance leads to lower distance value
        -- When black has more kings, red is in runAway mode so it tried to maximize the overall distance, the greater the distance, the greater the distance value  
        sumOfDistances
            | length (redKings st) >= length (blackKings st) = attack (redKings st) (blackKings st)
            | otherwise = runAway (redKings st) (blackKings st)