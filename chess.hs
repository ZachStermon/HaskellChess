module Chess
( showpiece , getpiece
, showspot , getspot
, getboard , showboard
, setspotonboard , getspotonboard
, executemove , validmove
, format , split
, initial , stringtomove, getbotmove
, Color , Type , Piece , Spot , Board , Move
) where

data Color = White | Black deriving (Show, Eq)
data Type = Pawn | Knight | Bishop | Rook | Queen | King deriving (Show, Eq)
data Piece = Piece Type Color deriving (Show, Eq)
type Spot = Maybe Piece
type Board = [Spot]
type Move = (Int, Int)
data Player = Player {name :: String, age :: Int} deriving (Show)
data Game = Game {w :: Player, b :: Player} deriving (Show)

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
  type player
  IO prompts
  alternate turns
  fix main
TODO
  **valid move checking**
  Random bot??
-}

{-
board representation is a single vector:
8 | 0  1  2  3  4  5  6  7  | 8
7 | 8  9  10 11 12 13 14 15 | 7
6 | 16 17 18 19 20 21 22 23 | 6
5 | 24 25 26 27 28 29 30 31 | 5
4 | 32 33 34 35 36 37 38 39 | 4
3 | 40 41 42 43 44 45 46 47 | 3
2 | 48 49 50 51 52 53 54 55 | 2
1 | 56 57 58 59 60 61 62 63 | 1
    A  B  C  D  E  F  G  H
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


ctr :: Char -> Int
ctr x = abs ((read [x]) - 8)

ctc :: Char -> Int
ctc 'a' = 0
ctc 'b' = 1
ctc 'c' = 2
ctc 'd' = 3
ctc 'e' = 4
ctc 'f' = 5
ctc 'g' = 6
ctc 'h' = 7
ctc 'A' = 0
ctc 'B' = 1
ctc 'C' = 2
ctc 'D' = 3
ctc 'E' = 4
ctc 'F' = 5
ctc 'G' = 6
ctc 'H' = 7

--input will look like a2 c6

stringtomove :: String -> Move
stringtomove s = (ctr (s!!1) * 8 + ctc (s!!0), ctr (s!!3) * 8 + ctc (s!!2))

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

inrange :: Move -> Bool
inrange x = ((fst x >= 0 && fst x <= 63) && (snd x >= 0 && snd x <= 63))

exists :: Board -> Move -> Bool
exists b m = not (showspot (b !! (fst m)) == '-')

changed :: Move -> Bool
changed m = not(fst x == snd x)

notfriendlyfire :: Board -> Move -> Bool
notfriendlyfire b m = not ((getspotonboard b (fst m)) == (getspotonboard b (snd m)))

correctturn :: Board -> Move -> Bool -> Bool
correctturn b m True
correctturn b m False

{-
* check if O and D are in range 0-63
* check if O is empty/null
check if its the correct for the person moving
* check if O==D
check if friendly fire is happening

*test if a piece is in the way (not for knights)*
***test movement type is right***
-}

validmove :: Board -> Move -> Bool -> Bool
validmove b m t = (inrange m) && (exists b m) && (correctturn b m t) && (changed m) && notfriendlyfire (b m)

validmove :: Board -> Move -> Spot -> Bool
validmove b m (Just (Piece Pawn White))   = True
validmove b m (Just (Piece Pawn Black))   = True
validmove b m (Just (Piece Knight _))     = True
validmove b m (Just (Piece Bishop _))     = True
validmove b m (Just (Piece Rook _))       = True
validmove b m (Just (Piece Queen _))      = True
validmove b m (Just (Piece King _))       = True
validmove _ _ _                           = False

getrow :: Int -> Int
getrow i = quot i 8

getcol :: Int -> Int
getcol i = mod i 8


getbotmove :: Board -> Int -> Int -> Move
getbotmove b a1 a2 = (a1,a2)


format :: String -> [String]
format s = split 8 s

split :: Int -> [a] -> [[a]]
split n = takeWhile (not.null) . map (take n) . iterate (drop n)
