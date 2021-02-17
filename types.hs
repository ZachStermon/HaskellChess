module Types where

import Data.Sequence



-- data types
data Type = Pawn | Knight | Bishop | Rook | Queen | King deriving (Show, Eq)
data Piece = Piece Type Bool deriving (Show, Eq)
type Spot = Maybe Piece
type Board = Seq Spot
type Move  = (Position, Position)
type Position = Int
type Side = Bool
type Turn = Bool



-- "global variables record"
data State = State { turn :: Bool,
                     board :: Board,
                     history :: [Move],
                     whitepieces :: Seq Position,
                     blackpieces :: Seq Position,
                     whiteattacks :: Seq Position,
                     blackattacks :: Seq Position,
                     wl :: Bool,
                     ws :: Bool,
                     bl :: Bool,
                     bs :: Bool
                    } deriving (Eq)
