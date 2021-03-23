module ApplyMove where

import Checkers.Types
import Moves

-- | The 'apply_move' function applies a move if valid, otherwise the move is not applied and a message is displayed saying why
apply_move :: Move -> GameState -> GameState
-- If there are no jumpMoves and simpleMoves for the player, update state to show the game is over
-- If the move is in the list of valid jump moves, make the jump move, while updating state
-- If there are no valid jumpmoves and the chosen move is a valid simplemove, make the simple move while updating state
-- If the chosen move is a simple move (and there are jump moves), let the user know they must play a jump move
-- Otherwise the chosen move is illegal and the user is informed of that
apply_move mv st
  | (null jumpMoves) && (null simpleMoves) = st {message = winner, status = GameOver}
  | mv `elem` jumpMoves = nextTurn (makeJumpMove mv (st {history = mv : (history st)}))
  | (null jumpMoves) && (mv `elem` simpleMoves) = nextTurn (makeSimpleMove [head mv, last mv] st {history = mv : (history st)})
  | (mv `elem` simpleMoves) = st {message = "Illegal move! A jump is available:  " ++ show jumpMoves}
  | otherwise = st {message = "Illegal move!!"}
  where
    simpleMoves = fst (moves st)
    jumpMoves = snd (moves st)
    nextTurn state
      | status st == Turn Red = state {status = Turn Black, message = ""}
      | status st == Turn Black = state {status = Turn Red, message = ""}
    winner
      | status st == Turn Red = "Game Over! Black wins!"
      | status st == Turn Black = "Game Over! Red wins!"

-- | The 'makeSimpleMove' function peforms a valid simple move and updates the game state
makeSimpleMove :: Move -> GameState -> GameState
-- Depending on whose turn it is, if the start Coordinate is in the player's kings list, replace the startCoord with the end Coord in that list
-- Otherwise if its a simple pawn move, call replacePiece to update the state accordingly
makeSimpleMove [start, end] st
  | status st == Turn Red && startCoord `elem` redKings st =
    st {redKings = replace startCoord endCoord (redKings st)}
  | status st == Turn Black && startCoord `elem` blackKings st =
    st {blackKings = replace startCoord endCoord (blackKings st)}
  | otherwise = replacePiece startCoord end st
  where
    startCoord = toCoord start
    endCoord = toCoord end

-- | The 'replacePiece' function replaces a Coord with another PorK Coord, used for updating the state after a move
replacePiece :: Coord -> PorK Coord -> GameState -> GameState
-- If the end of the move is a Pawn, then according to whose turn it is, replace the start with the end in the current player's pieces list
-- If the end of the move is a King, the pawn has converted to a king
-- The end Coord should be prepended to the current player's kings list, and the start should be removed from the pieces list
replacePiece start (P end) st
  | status st == Turn Red = st {redPieces = replace start end (redPieces st)}
  | status st == Turn Black = st {blackPieces = replace start end (blackPieces st)}
replacePiece start (K end) st
  | status st == Turn Red =
    st
      { redKings = end : redKings st,
        redPieces = remove start (redPieces st)
      }
  | status st == Turn Black =
    st
      { blackKings = end : blackKings st,
        blackPieces = remove start (blackPieces st)
      }

-- | The 'toCoord' function takes in a PorK Coord and returns the Coord value
toCoord :: PorK Coord -> Coord
toCoord (P x) = x
toCoord (K x) = x

-- | The 'makeJumpMove' function peforms a valid jump move and updates the game state
makeJumpMove :: Move -> GameState -> GameState
-- If there is only one Coord in the list, the jump has ended, so the state is returned
-- Otherwise if there are at least 2 Coords in the move, if the start Coord is in the current player's kings list, call makeJumpMove and update the state
-- by removing the jumped over gamepiece from either the opponent's kings or pieces lists, and prepend the landing or next Coord to their kings list
-- If the start coord is in the current player's pieces list, call makeJumpMove and remove the jumped gamepiece from either their opponents lists
-- In order to know where to add the landing spot or next coord, replacePiece is called, as it could convert to a King, and the state is updated accordingly
makeJumpMove [end] st = st
makeJumpMove (start : (next : rest)) st
  | status st == Turn Red && startCoord `elem` redKings st =
    makeJumpMove
      (next : rest)
      ( st
          { blackKings = remove (jumped startCoord nextCoord) (blackKings st),
            blackPieces = remove (jumped startCoord nextCoord) (blackPieces st),
            redKings = nextCoord : remove startCoord (redKings st)
          }
      )
  | status st == Turn Black && startCoord `elem` blackKings st =
    makeJumpMove
      (next : rest)
      ( st
          { redKings = remove (jumped startCoord nextCoord) (redKings st),
            redPieces = remove (jumped startCoord nextCoord) (redPieces st),
            blackKings = nextCoord : remove startCoord (blackKings st)
          }
      )
  | status st == Turn Red && startCoord `elem` redPieces st =
    makeJumpMove
      (next : rest)
      ( (replacePiece startCoord next st)
          { blackKings = remove (jumped startCoord nextCoord) (blackKings st),
            blackPieces = remove (jumped startCoord nextCoord) (blackPieces st)
          }
      )
  | status st == Turn Black && startCoord `elem` blackPieces st =
    makeJumpMove
      (next : rest)
      ( (replacePiece startCoord next st)
          { redKings = remove (jumped startCoord nextCoord) (redKings st),
            redPieces = remove (jumped startCoord nextCoord) (redPieces st)
          }
      )
  where
    startCoord = toCoord start
    nextCoord = toCoord next

-- | The 'jumped' function returns the the coordinates of the jumped over square given the start and end coordinates of a jump
jumped :: (Integral a, Integral b) => (a, b) -> (a, b) -> (a, b)
jumped (x, x') (y, y') = ((x + y) `div` 2, (x' + y') `div` 2)