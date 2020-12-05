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
  not crash program with invalid move
  valid move checking
TODO
  Random bot??
  checking if moving through a piece
POSSIBLE TODO
  checkmate
  stalemate
  get list of valid moves
  getscore
  in check after move

ITS A REACH
  minimax
  AB pruning
-}

{-
board representation is a single vector:
8 | 0  1  2  3  4  5  6  7  | 8 0
7 | 8  9  10 11 12 13 14 15 | 7 1
6 | 16 17 18 19 20 21 22 23 | 6 2
5 | 24 25 26 27 28 29 30 31 | 5 3
4 | 32 33 34 35 36 37 38 39 | 4 4
3 | 40 41 42 43 44 45 46 47 | 3 5
2 | 48 49 50 51 52 53 54 55 | 2 6
1 | 56 57 58 59 60 61 62 63 | 1 7
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
inrange x = ((fst x) >= 0 && (fst x) <= 63) && ((snd x) >= 0 && (snd x) <= 63)

exists :: Board -> Int -> Bool
exists b i = (showspot (b !! i) /= '-')

changed :: Move -> Bool
changed m = (fst m) /= (snd m)

notfriendlyfire :: Board -> Move -> Bool
notfriendlyfire b m = ((getcolor (getspotonboard b (fst m))) /= (getcolor (getspotonboard b (snd m))))

correctturn :: Board -> Int -> Bool -> Bool
correctturn b i True  = getcolor (b !! i) == White
correctturn b i False = getcolor (b !! i) == Black

getcolor :: Spot -> Color
getcolor (Just (Piece _ White)) = White
getcolor (Just (Piece _ Black)) = Black
getcolor _                      = undefined

colortobool :: Color -> Bool
colortobool White = True
colortobool Black = False

isenemy :: Board -> Int -> Bool -> Bool
isenemy b i t = (exists b i) && (colortobool (getcolor (b !! i)) /= t)

{-
* check if O and D are in range 0-63
* check if O is empty/null
* check if its the correct for the person moving
* check if O==D
* check if friendly fire is happening

*test if a piece is in the way (not for knights)*
***test movement type is right***
-}

validmove :: Board -> Move -> Bool -> Bool
validmove b m t = (validmove' b m (getspotonboard b (fst m))) && (inrange m)
                  && (exists b (fst m)) && (correctturn b (fst m) t) && (changed m)
                  && (if (exists b (snd m)) then (notfriendlyfire b m) else True)

getvalidmoves :: Board -> Bool -> Int -> Int -> [Move]
getvalidmoves b t i1 i2 = if (validmove b (i1,i2) t) then xs: else getvalidmoves b t i1 (i2 + 1)

validmove' :: Board -> Move -> Spot -> Bool
validmove' b m (Just (Piece Pawn White))   = pmovementw b m
validmove' b m (Just (Piece Pawn Black))   = pmovementb b m
validmove' b m (Just (Piece Knight _))     = nmovement m
validmove' b m (Just (Piece Bishop _))     = (bmovement m) && (notblocked b m False)
validmove' b m (Just (Piece Rook _))       = (rmovement m) && (notblocked b m True)
validmove' b m (Just (Piece Queen _))      = ((rmovement m) && (notblocked b m True))|| ((bmovement m) && (notblocked b m False))
validmove' b m (Just (Piece King _))       = kmovement m
validmove' _ _ _                           = False


pmovementw :: Board -> Move -> Bool
pmovementw b m = ((col (fst m) == col (snd m)) && (row (fst m) - row (snd m) == 1) && (not (exists b (snd m)))) --if columns equal eachother and (destination row - origin row = 1) and no piece exists at destination
              || ((col (fst m) == col (snd m)) && (row (fst m) - row (snd m) == 2) && (not (exists b (snd m))) && (not (exists b ((fst m) - 8))) && (row(fst m) == 6)) --this line is for first pawn move double move
              || (((fst m) - (snd m) == 9) && (isenemy b (snd m) True) && (col (fst m) /= 0)) --upleft -9
              || (((fst m) - (snd m) == 7) && (isenemy b (snd m) True) && (col (fst m) /= 7)) --upright -7

pmovementb :: Board -> Move -> Bool
pmovementb b m = ((col (fst m) == col (snd m)) && (row (snd m) - row (fst m) == 1) && (not (exists b (snd m)))) --one pawn move forward
              || ((col (fst m) == col (snd m)) && (row (snd m) - row (fst m) == 2) && (not (exists b (snd m))) && (not (exists b ((fst m) + 8))) && (row (fst m) == 1)) --doublepawn move
              || (((snd m) - (fst m) == 7) && (isenemy b (snd m) False) && (col (fst m) /= 0)) --downleft +7
              || (((snd m) - (fst m) == 9) && (isenemy b (snd m) False) && (col (fst m) /= 7)) --downright +9


nmovement :: Move -> Bool
nmovement m = ((abs (row (fst m) - row (snd m)) * (abs (col (fst m) - col (snd m)))) == 2)

bmovement :: Move -> Bool
bmovement m = (abs(row(fst m) - row (snd m)) == abs(col(fst m) - col(snd m)))

kmovement :: Move -> Bool
kmovement m = (abs (row (fst m) - row (snd m)) <= 1 && (abs (col (fst m) - col (snd m))) <= 1)

rmovement :: Move -> Bool
rmovement m = ((row(fst m)) == (row(snd m))) /= (((col(fst m)) == (col(snd m))))

--This bool is NOT side it is the type of movement True = Rook movement, False = Bishop Movement
notblocked :: Board -> Move -> Bool -> Bool
notblocked b m type = True


row :: Int -> Int
row i = quot i 8

col :: Int -> Int
col i = mod i 8


getbotmove :: Board -> Int -> Int -> Move
getbotmove b a1 a2 = (a1,a2)


format :: String -> [String]
format s = split 8 s

split :: Int -> [a] -> [[a]]
split n = takeWhile (not.null) . map (take n) . iterate (drop n)
