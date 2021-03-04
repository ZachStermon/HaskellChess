module Types where

import Data.Sequence
import Data.Word (Word64, Word8)

data Piece = Void
           | BlackPawn
           | BlackKnight
           | BlackBishop
           | BlackRook
           | BlackQueen
           | BlackKing
           | WhitePawn
           | WhiteKnight
           | WhiteBishop
           | WhiteRook
           | WhiteQueen
           | WhiteKing deriving (Show, Eq, Enum, Ord)

type Spot = Maybe Piece
type Board = Seq Piece
type Move  = (Position, Position)
type Position = Word8
type Side = Bool



data BitBoard = BitBoard { whitepawns   :: Word64,
                           blackpawns   :: Word64,
                           whiteknights :: Word64,
                           blackknights :: Word64,
                           whitebishops :: Word64,
                           blackbishops :: Word64,
                           whiterooks   :: Word64,
                           blackrooks   :: Word64,
                           whitequeens  :: Word64,
                           blackqueens  :: Word64,
                           whitekings   :: Word64,
                           blackkings   :: Word64,
                           whitepieces  :: Word64,
                           blackpieces  :: Word64,
                           occupied     :: Word64} deriving (Eq, Ord)

-- ptp :: Piece -> (BitBoard -> Word64)
-- ptp Void        = error("empty piece in ptp")
-- ptp BlackPawn   = blackpawns
-- ptp BlackKnight = blackknights
-- ptp BlackBishop = blackbishops
-- ptp BlackRook   = blackrooks
-- ptp BlackQueen  = blackqueens
-- ptp BlackKing   = blackkings
-- ptp WhitePawn   = whitepawns
-- ptp WhiteKnight = whiteknights
-- ptp WhiteBishop = whitebishops
-- ptp WhiteRook   = whiterooks
-- ptp WhiteQueen  = whitequeens
-- ptp WhiteKing   = whitekings




-- "global variables record"
data State = State { turn         :: Bool,
                     board        :: BitBoard,
                     history      :: [Move],
                     whiteattacks :: Word64,
                     blackattacks :: Word64,
                     wl :: Bool,
                     ws :: Bool,
                     bl :: Bool,
                     bs :: Bool
                    }
