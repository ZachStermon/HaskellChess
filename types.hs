module Types where

-- data types
data Type = Pawn | Knight | Bishop | Rook | Queen | King deriving (Show, Eq)
data Piece = Piece Type Bool deriving (Show, Eq)
type Spot = Maybe Piece
type Board = [Spot]
type Move  = (Position, Position)
type Position = Int
type Side = Bool
type Turn = Bool


-- "global variables record"
data State = State { turn :: Bool,
                     board :: Board,
                     history :: [Move],
                     blackcanlongcastle :: Bool,
                     blackcanshortcastle :: Bool,
                     whitecanlongcastle :: Bool,
                     whitecanshortcastle :: Bool,
                     capturedpieces :: [Piece]
                    } deriving (Show, Eq)
