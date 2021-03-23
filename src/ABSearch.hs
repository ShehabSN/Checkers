module ABSearch where

import Checkers.Types
import Moves
import ApplyMove
import Heuristic

-- | The 'red_ai' function takes in a gamestate and returns a move it evaluates to be a good move for red to take given the current state
red_ai :: GenMove 
red_ai state = snd (alphabeta state 10 minBound maxBound True (Turn Red))

-- | The 'ai' function takes in a gamestate and returns a move it evaluates to be a good move for black to take given the current state
black_ai :: GenMove 
black_ai state = snd (alphabeta state 10 minBound maxBound True (Turn Black))

-- | The 'computeHeuristic' function calculates a value using a heuristic function, depending on the gamestate and whose turn it is
computeHeuristic :: GameState -> Status -> (Int, Move)
computeHeuristic state turn
    | turn == Turn Black = (blackHeuristic state, [])
    | turn == Turn Red = (redHeuristic  state, [])

-- | The 'alphaBeta' function returns the highest valued move (and its value) the player whose turn it is can take using AB Pruning and MinMax
-- | Inspired by Niran's implementation of AB Pruning for a game of Tic Tac Toe
-- If the depth <= 0 and there are no jump moves, compute the heuristic. This allows for a quiescence search at depth 0 until there are no jump moves (and a quiet state is reached)
-- If there are no moves left (gameover) and we are currently maximizing, returning the minBound value to represent a very unappealing state to avoid at all costs
-- Similarly, if there are no moves left, and we are minimizing, return the maxBound to represent a very unappealing state to avoid at all costs
-- If we are maximizing, maximize is called with the list of moves to search, and the pair of alpha, and the pair of initial move value (minBound) and an empty move
-- Otherwise, if we are minimizing, minimize is called with the list of moves to search, and the pair of alpha, and the pair of initial move value (maxBound) and an empty move
alphabeta :: (Ord t, Num t) => GameState -> t -> Int -> Int -> Bool -> Status -> (Int, Move)
alphabeta state depth alpha beta maximizing turn
    | depth <= 0 && null jumpMoves = computeHeuristic state turn
    | (simpleMoves, jumpMoves) == ([],[]) && maximizing = (minBound , [])
    | (simpleMoves, jumpMoves) == ([],[]) && not maximizing = (maxBound, [])
    | maximizing = snd $ maximize movesToSearch (alpha, (minBound, []))
    | otherwise = snd $ minimize movesToSearch (beta, (maxBound, []))
    where
        -- | The 'maximize' function given a list of moves, maximizes and returns a pair with the updated alpha, and a pair representing the highest (by maximizing) value and move
        -- If there are no moves, return the value of alpha, and the pair of value and move
        -- Otherwise if alpha >= beta, we can prune that path and return the value of alpha, and the pair of value and move
        -- Otherwise call maximize again on the remaining moves with an updated pair passed in, as evaluated from a call to alphabeta, now minimizing, with an updated state after move is applied
        maximize:: [Move] -> (Int, (Int, Move)) -> (Int, (Int, Move))
        maximize [] ret = ret
        maximize (m:ms) (prevAlpha, (prevValue, prevMove)) 
            |  alpha >= beta = (newAlpha, (value, move))
            | otherwise = maximize ms (newAlpha, (value, move))
            where 
                (eval, _) = alphabeta (apply_move m state) (depth - 1)  prevAlpha beta False turn
                value = max prevValue eval
                newAlpha = max prevAlpha value
                move = if prevValue == value then prevMove else m
        -- | The 'minimize' function given a list of moves, minimizes and returns a pair with the updated beta, and a pair representing the highest (by minimizing) value and move
        -- If there are no moves, return the value of beta, and the pair of value and move
        -- Otherwise if beta <= alpha, we can prune that path and return the value of beta, and the pair of value and move
        -- Otherwise call minimize again on the remaining moves with an updated pair passed in, as evaluated from a call to alphabeta, now maximizing, with an updated state after move is applied
        minimize :: [Move] -> (Int, (Int, Move)) -> (Int, (Int, Move))
        minimize [] ret = ret
        minimize (m:ms) (prevBeta, (prevValue, prevMove)) 
            | beta <= alpha  = (newBeta, (value, move))
            | otherwise = minimize ms (newBeta, (value, move))
            where 
                (eval, _) = alphabeta (apply_move m state) (depth - 1) alpha prevBeta True turn
                value = min prevValue eval
                newBeta = min prevBeta value
                move = if prevValue == value then prevMove else m
        (simpleMoves, jumpMoves) = moves state
        -- if there are jump moves, only those should be considered, otherwise simple moves are considered  
        movesToSearch = case (simpleMoves, jumpMoves) of
            (_, x:xs) -> x:xs
            (xs, []) -> xs
