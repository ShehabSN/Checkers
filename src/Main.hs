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
        blackMove = Ai ai,
        redMove = Human,
        state = initialGameState 
      }

statetest2 :: GameState
statetest2 = GameState {blackPieces = [(6,1)], redPieces = [(5,4),(2,1),(6,7)], blackKings = [], redKings = [], status = Turn Black, message = "", history = [[P (7,6),P (5,4)],[K (4,7),K (6,5)],[K (6,5),K (5,6)],[P (7,0),P (6,1)],[K (5,6),K (6,5)],[K (3,6),K (4,7)],[K (6,5),K (5,6)],[K (2,7),K (3,6)],[K (7,4),K (6,5)],[K (3,6),K (2,7)],[K (6,3),K (7,4)],[K (2,7),K (3,6)],[K (7,2),K (6,3)],[K (3,6),K (2,7)],[P (1,2),P (2,1)],[K (2,7),K (3,6)],[P (0,3),P (1,2)],[P (3,6),K (2,7)],[K (5,0),K (7,2)],[P (4,5),P (3,6)],[P (4,1),K (5,0)],[P (5,4),P (4,5)],[P (3,2),P (4,1)],[P (7,2),P (5,4)],[P (5,4),P (3,2)],[P (3,2),P (4,3)],[P (7,4),P (6,3)],[P (5,0),P (3,2)],[K (2,3),K (4,1)],[K (4,1),K (3,2)],[K (3,2),K (2,3)],[K (5,2),K (4,1)],[K (2,1),K (3,2)],[K (4,1),K (5,2)],[K (1,0),K (2,1)],[K (5,2),K (4,1)],[P (3,2),K (1,0)],[K (1,6),K (3,4),K (5,2)],[P (2,3),P (3,2)],[K (2,7),K (1,6)],[P (3,4),P (2,3)],[P (1,6),K (2,7)],[P (4,5),P (3,4)],[P (0,5),P (1,6)],[P (2,7),P (4,5)],[P (4,5),P (3,6)],[P (3,6),P (2,5)],[P (3,4),P (4,5)],[P (5,6),P (7,4)],[P (7,4),P (6,5)],[P (6,5),P (5,4)],[P (6,3),P (7,4)],[P (4,7),P (3,6)],[P (5,2),P (6,3)],[P (5,4),P (4,3)],[P (3,0),P (5,2)],[P (2,3),P (4,1)],[P (5,2),P (3,4)],[P (1,4),P (2,3)],[P (4,1),P (5,2)],[P (2,5),P (4,3)],[P (4,3),P (3,4)],[P (3,6),P (5,4)],[P (2,3),P (4,5)],[P (4,5),P (3,4)],[P (5,2),P (4,3)],[P (1,6),P (2,5)],[P (0,1),P (2,3)],[P (0,7),P (1,6)],[P (1,0),P (0,1)],[P (3,4),P (1,2)],[P (1,2),P (2,3)],[P (2,5),P (1,4)],[P (0,1),P (1,2)],[P (1,6),P (2,5)],[P (1,4),P (0,5)],[P (2,5),P (3,4)],[P (2,3),P (1,4)],[P (1,4),P (0,3)],[P (1,2),P (2,3)],[P (0,5),P (1,4)]]}

g1 =
  GameState
    { blackPieces = [(1,3), (3, 1),(2,0)],
      redPieces = [(2,4),(4,6),(6,6),(6,4)],
      blackKings = [(3,3)],
      redKings = [],
      status = Turn Red,
      message = "",
      history = []
    }

g2 =
  GameState
    { blackPieces = [(6, 1), (4, 1), (2, 1)],
      redPieces = [],
      blackKings = [],
      redKings = [(7, 2)],
      status = Turn Red,
      message = "",
      history = []
    }

g3 =
  GameState
    { blackPieces = [(6, 3), (6, 1), (4, 1), (2, 1)],
      redPieces = [],
      blackKings = [],
      redKings = [(5, 4)],
      status = Turn Red,
      message = "",
      history = []
    }

g4 =
  GameState
    { blackPieces = [(4, 3), (6, 3), (6, 1), (4, 1), (2, 1)],
      redPieces = [],
      blackKings = [],
      redKings = [(3, 2)],
      status = Turn Red,
      message = "",
      history = []
    }

-- Black move to test absearch

g5 =
  GameState
    { blackPieces = [(7, 0)],
      redPieces = [(4, 5), (6, 5)],
      blackKings = [(4, 7)],
      redKings = [(3, 0)],
      status = Turn Black,
      message = "",
      history = []
    }

--Red move  move to test absearch

g6 =
  GameState
    { blackPieces = [(7, 0)],
      redPieces = [(4, 5), (6, 5), (4, 3), (6, 3)],
      blackKings = [(5, 6)],
      redKings = [(3, 0)],
      status = Turn Red,
      message = "",
      history = []
    }

-- red move to win!

g7 =
  GameState
    { blackPieces = [(7, 6)],
      redPieces = [],
      blackKings = [(0, 7)],
      redKings = [(1, 4), (6, 7)],
      status = Turn Red,
      message = "",
      history = []
    }

t1Black =
  GameState
    { blackPieces = [(3, 3), (5, 3), (5, 5)],
      redPieces = [(4, 4)],
      blackKings = [],
      redKings = [],
      status = Turn Black,
      message = "",
      history = []
    }

t2Black =
  GameState
    { blackPieces = [(3, 3), (5, 3), (5, 5)],
      redPieces = [(4, 4), (2, 6)],
      blackKings = [],
      redKings = [],
      status = Turn Black,
      message = "",
      history = []
    }

t3Black =
  GameState
    { blackPieces = [(6, 6)],
      redPieces = [(7, 7)],
      blackKings = [],
      redKings = [],
      status = Turn Black,
      message = "",
      history = []
    }

t4Black =
  GameState
    { blackPieces = [(3, 3), (5, 3), (5, 5)],
      redPieces = [(4, 4), (2, 6), (4, 6), (6, 6)],
      blackKings = [],
      redKings = [],
      status = Turn Black,
      message = "",
      history = []
    }

-- Pawn changing to King

t5Black =
  GameState
    { blackPieces = [(4, 6)],
      redPieces = [(3, 3)],
      blackKings = [],
      redKings = [],
      status = Turn Black,
      message = "",
      history = []
    }

--non-loopy jumps

t6Black =
  GameState
    { blackPieces = [(3, 3), (5, 3), (5, 5)],
      redPieces = [(4, 4), (2, 6), (6, 6)],
      blackKings = [],
      redKings = [],
      status = Turn Black,
      message = "",
      history = []
    }

--Initial Game GameState

t7Black = initialGameState
