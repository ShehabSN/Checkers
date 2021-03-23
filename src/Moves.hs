module Moves where

import Checkers.Types

-- | The 'moves' function takes in a gamestate and returns the valid simple and jump moves the player can take
moves :: GameState -> ([Move], [Move])
moves st = (simpleMoves st, jumpMoves st)

-- | The 'simpleMoves' function takes in a gamestate and returns a list of valid simple moves the player can take
simpleMoves :: GameState -> [Move]
-- The simple moves a king and piece can take are computed and returned for the current player whose turn it is
-- If the game is over, return an empty list
simpleMoves st
  | status st == Turn Red =
    simpleKing (redKings st) ++ simplePiece (redPieces st) (-1)
  | status st == Turn Black =
    simpleKing (blackKings st) ++ simplePiece (blackPieces st) 1
  | otherwise = []
  where
    -- Returns the simple moves a piece can take by checking each coord adjacent to the piece in the given vertical direction
    -- The adjacent tiles must not be occupied and must be on the board
    -- Since a king transformation is possible, the correct type of PorK coord is computed and returned with checkKingConversion
    simplePiece xs dir =
      [ [P (x, y), checkKingConversion (x', y')]
        | (x, y) <- xs,
          (x', y') <- let y' = y + dir in [(x + 1, y'), (x -1, y')],
          onBoard (x', y') && notOccupied (x', y') st
      ]
    -- Returns the simple moves a king can take by checking each coord adjacent to the piece in all directions
    -- The adjacent tiles must not be occupied, must be on the board, and must not cause a repeated state
    simpleKing xs =
      [ [K (x, y), K (x', y')]
        | (x, y) <- xs,
          (x', y') <- [(x + 1, y + 1), (x -1, y + 1), (x + 1, y -1), (x -1, y -1)],
          onBoard (x', y') && notOccupied (x', y') st && doesNotRepeat (history st) (K (x, y), K (x', y'))
      ]

-- | The 'doesNotRepeat' function takes in a list of Moves, and a simple Move (as a pair) and checks that the move won't cause a repeated state
-- | A repeated state is present if running the Moves history back returns an empty list (is null), otherwise there is no repeated state
doesNotRepeat :: (Num a, Eq a, Eq b) => [[PorK (a, b)]] -> (PorK (a, b), PorK (a, b)) -> Bool
doesNotRepeat history mvs
  | null (runHistoryBack history [mvs]) = False
  | otherwise = True
  where
    -- If mvs list is empty (second argument), that means a repeated state occured, and thus an empty list should be returned
    -- If the history has been iterated through (an empty list is given for first argument),
    -- or if the first move of the moves list is a pawn move, then return the moves list since the state then and before cant repeat
    -- If the absolute value difference between the x coordinates of the first 2 King Coords in the first move of the current history list is equal to 1
    -- This means the move must be a simple King move, thus call runHistoryBack with the rest of the history and an updated mvs list returned from evaluateMove
    -- Otherwise it must be a jump move and the moves list should be returned since the state then and before cant repeat
    runHistoryBack _ [] = []
    runHistoryBack [] mvs = mvs
    runHistoryBack (((P _) : xs) : ys) mvs = mvs
    runHistoryBack ((K (x, y) : K (x', y') : xs) : ys) mvs
      | abs (x - x') == 1 = runHistoryBack ys (evaluateMove [K (x, y), K (x', y')] mvs)
      | otherwise = mvs

-- | The 'evaluateMove' takes in a given move from the history and evaluates how it should update the movements list
-- | This algorithm was described by Dr.Cockett and acts as a sort of state machine to discern whether a move will cause a repeated state
evaluateMove :: Eq a => [PorK a] -> [(PorK a, PorK a)] -> [(PorK a, PorK a)]
-- If the moves list is empty or there is no match, prepend the move from history to the movements list
-- A match is when the movements list contains a move such that its start Coord equals the end Coord of the move from history
-- Otherwise update the moves list
-- If the start of the move from history equals the end of the matched move from movements list, remove the move from movevements list
-- Otherwise, simply update the start element of the move in movements list to the start of the move from history (i.e the earlier start)
evaluateMove ((K start) : (K end) : xs) moves
  | null moves || null matching = (K start, K end) : moves
  | otherwise = updateMoves (head matching) moves
  where
    matching = filter (\(st, _) -> st == K end) moves
    updateMoves (x, y) moves
      | K start == y = filter (/= (x, y)) moves
      | otherwise = replace (x, y) (K start, y) moves

-- | The 'onBoard' function takes in a Coord and checks whether it represents a valid square on the game board
onBoard :: Coord -> Bool
onBoard (x, y)
  | x >= 0 && x <= 7 && y >= 0 && y <= 7 = True
  | otherwise = False

-- | The 'notOccupied' function checks whether a Coord on the board is not occupied by a game Piece, using the given gamestate
notOccupied :: Coord -> GameState -> Bool
-- A Coord is not occupied if it is not present in the lists of redPieces, blackPieces, blackKings, and redKings
notOccupied coord st
  | notElem coord (redPieces st) && notElem coord (blackPieces st) && notElem coord (blackKings st) && notElem coord (redKings st) = True
  | otherwise = False

-- | The 'checkKingConversion' function constructs a PorK Coord depending on whether the Coord is in position to convert to a King or not
checkKingConversion :: Coord -> PorK Coord
-- If the Coord is on either vertical end of the board (y is 0 or 7), then it is in position to become a King
checkKingConversion (x, y)
  | y == 0 || y == 7 = K (x, y)
  | otherwise = P (x, y)

-- | The 'jumpMoves' function takes in a gamestate and returns a list of valid jump moves the player can take
jumpMoves :: GameState -> [Move]
-- The jump moves a king and piece can take are computed and returned for the current player whose turn it is
-- If the game is over, return an empty list
jumpMoves st
  | status st == Turn Red =
    jumpKing (redKings st) ++ jumpPiece (redPieces st) (-1)
  | status st == Turn Black =
    jumpKing (blackKings st) ++ jumpPiece (blackPieces st) 1
  | otherwise = []
  where
    -- For ever Coord in a given list, it is prepended to every valid king jump moves combination starting from that Coord
    jumpKing xs = [K (x, y) : ys | (x, y) <- xs, ys <- jumpKing' (x, y) [] (x, y)]
    -- The end position of every valid adjacent king jump move starting from (x,y) is prepended to
    -- the result of recursive call to find the next valid jump moves starting from the current end position
    -- The jumped over gamepieces are prepended to the list (rem) to avoid jumping over them again
    -- The starting position of the jump is recorded to allow for jumping and ending at the start position
    -- For a jump move to be valid, the adjacent position must have an opponent, the opponent being jumped over mustn't have been jumped over before,
    -- The landing/end position can be the starting position of the initial jump or it must not be occupied, and it must be on the board
    jumpKing' start rem (x, y) =
      [ K (x'', y'') : ys
        | ((x', y'), (x'', y'')) <- [((x + 1, y + 1), (x + 2, y + 2)), ((x -1, y + 1), (x -2, y + 2)), ((x + 1, y -1), (x + 2, y -2)), ((x -1, y -1), (x -2, y -2))],
          opponentOccupied (x', y') st && notElem (x', y') rem && (start == (x'', y'') || notOccupied (x'', y'') st) && onBoard (x'', y''),
          ys <- jumpOver (jumpKing' start ((x', y') : rem) (x'', y''))
      ]
    -- For ever Coord in a given list, it is prepended to every valid piece jump moves combination starting from that Coord
    jumpPiece xs dir = [P (x, y) : ys | (x, y) <- xs, ys <- jumpPiece' (x, y) [] dir]
    -- The end position of every valid adjacent (vertically based on dir) piece jump move starting from (x,y) is prepended to
    -- the result of recursive call to find the next valid jump moves starting from the current end position
    -- For a jump move to be valid, the adjacent position must have an opponent, the landing/end position must not be occupied, and it must be on the board
    -- Since a piece can convert to a King during a jump move, checkKingConversion is required for correct PorK type,
    -- The list of jumped over gamepieces is also tracked in the case of a conversion to King, where more jump moves are available
    -- Hence, jumpAs checks whether to continue looking for jump moves as a piece, or to call jumpKing' and continue looking for jumps as a King
    jumpPiece' (x, y) rem dir =
      [ checkKingConversion (x'', y'') : ys
        | ((x', y'), (x'', y'')) <- [((x + 1, y + dir), (x + 2, y + 2 * dir)), ((x -1, y + dir), (x -2, y + 2 * dir))],
          opponentOccupied (x', y') st && notOccupied (x'', y'') st && onBoard (x'', y''),
          ys <- jumpOver (jumpAs ((x', y') : rem) (checkKingConversion (x'', y'')) dir)
      ]
    jumpAs rem (P (x, y)) dir = jumpPiece' (x, y) rem dir
    jumpAs rem (K (x, y)) dir = jumpKing' (x, y) rem (x, y)
    -- In the case that the jump is over as no more jumps are available, return a list of an empty list
    jumpOver [] = [[]]
    jumpOver z = z

-- | The 'opponentOccupied' function checks whether there's a gamepiece of the opposing player at a given Coord, using the game state
opponentOccupied :: Coord -> GameState -> Bool
-- If its currently Red's turn, check that the Coord isnt in the lists of blackPieces and blackKings
-- Otherwise it's Black's turn and thus check the lists of redPieces and redKings
opponentOccupied coord st
  | status st == Turn Red = elem coord (blackPieces st) || elem coord (blackKings st)
  | otherwise = elem coord (redPieces st) || elem coord (redKings st)

-- | The 'remove' function removes an element from a list
remove :: Eq a => a -> [a] -> [a]
remove x = filter (/= x)

-- | The 'replace' function removes an element and replaces it with another element (not necessarily in same location)
replace :: Eq b => b -> b -> [b] -> [b]
replace x y xs = y : remove x xs