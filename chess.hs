module Chess
( showpiece , getpiece
, showspot , getspot
, getboard , showboard
, setspotonboard , getspotonboard
, executemove , validmove
, format , split
, initial
, Color , Type , Piece , Spot , Board , Move
) where

data Color = White | Black deriving (Show)
data Type = Pawn | Knight | Bishop | Rook | Queen | King deriving (Show)
data Piece = Piece Type Color deriving (Show)
type Spot = Maybe Piece
type Board = [Spot]
type Move = (Int, Int)
type Player = {Name :: String, Age :: Int, ELO :: Int}
type Game = {W :: Player, B :: Player}

initial = getboard(init_)
init_ = init1 ++ init2 ++ init3 ++ init3 ++ init3 ++ init3 ++ init4 ++ init5
init1 = "rnbqkbnr"
init2 = "pppppppp"
init3 = "--------"
init4 = "PPPPPPPP"
init5 = "RNBQKBNR"

{-
DONE
  formatted printing Board
  moving pieces
  updating board
TODO
  ****fix main****
  **valid move checking**
  *IO prompts*
  alternate turns
  Random bot??
  type player
-}

showpiece :: Piece -> Char
showpiece (Piece Pawn White)   = 'P'
showpiece (Piece Knight White) = 'N'
showpiece (Piece Bishop White) = 'B'
showpiece (Piece Rook White)   = 'R'
showpiece (Piece Queen White)  = 'Q'
showpiece (Piece King White)   = 'K'
showpiece (Piece Pawn Black)   = 'p'
showpiece (Piece Knight Black) = 'n'
showpiece (Piece Bishop Black) = 'b'
showpiece (Piece Rook Black)   = 'r'
showpiece (Piece Queen Black)  = 'q'
showpiece (Piece King Black)   = 'k'

getpiece :: Char -> Maybe Piece
getpiece 'P' = Just (Piece Pawn White)
getpiece 'N' = Just (Piece Knight White)
getpiece 'B' = Just (Piece Bishop White)
getpiece 'R' = Just (Piece Rook White)
getpiece 'Q' = Just (Piece Queen White)
getpiece 'K' = Just (Piece King White)
getpiece 'p' = Just (Piece Pawn Black)
getpiece 'n' = Just (Piece Knight Black)
getpiece 'b' = Just (Piece Bishop Black)
getpiece 'r' = Just (Piece Rook Black)
getpiece 'q' = Just (Piece Queen Black)
getpiece 'k' = Just (Piece King Black)
getpiece  _  = Nothing

{-
getMove :: String -> Move
getMove [] =
-}

showspot :: Spot -> Char
showspot Nothing = '-'
showspot (Just piece) = showpiece piece

getspot :: Char -> Spot
getspot = getpiece

getboard :: String -> Board
getboard string = map getspot string

showboard :: Board -> String
showboard b = map showspot b

setspotonboard :: Board -> Spot -> Int -> Board
setspotonboard (x:xs) a num
  | num == 0 = a:xs
  | otherwise = x:setspotonboard xs a (num-1)

getspotonboard :: Board -> Int -> Spot
getspotonboard (xs) i = xs !! i

executemove :: Board -> Move -> Board
executemove b m = setspotonboard (setspotonboard b (getspotonboard b (fst m)) (snd m)) (getspot '-') (fst m)

validmove :: Board -> Move -> Bool
validmove _ _ = True

format :: String -> [String]
format s = split 8 s

split :: Int -> [a] -> [[a]]
split n = takeWhile (not.null) . map (take n) . iterate (drop n)

--putStr $ unlines $ format (showboard(executemove initial (9,33)))
