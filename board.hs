module board(Board, init, move, validmove, stringtomove) where

import qualified Data.Vector as vector

data pieces = p | n | b | r | q | k | x deriving (Show, Eq)

data piece = piece boolean pieces deriving Eq

--TODO make uppercase if white
instance Show piece where
  show (piece True p) =
  show (piece False p) = show p

data Board = Board {setup :: (vector.Vector (piece))}

data move = move {origin :: Int, dest :: Int}

init :: Board {
  setup = vector.fromList concat
  [Just piece True r, ]

}
