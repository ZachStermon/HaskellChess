module Chess
( showpiece , getpiece
, showspot
, getboard , showboard
, setspotonboard , getpieceonboard
, executemove , validmove
, format , split, getbotmove
, initial , stringtomove,  getmoves
, Type , Piece , Spot , Board , Move
, checkmate , stalemate
) where

import Weights
import Helpers

data Type = Pawn | Knight | Bishop | Rook | Queen | King deriving (Show, Eq)
data Piece = Piece Type Bool deriving (Show, Eq)
type Spot = Maybe Piece
type Board = [Spot]
type Move = (Int, Int)

staleboard =     getboard ("kr--------r-----------------------------r-------P-------K-------")
checkmateboard = getboard ("----k------------------------------q---------------PPb-----QKB--")
testboard =      getboard ("rnbqkb-rppp---pp-----------nNp-Q--B-------------PPPP-PPPRNB-K--R")
mateinthree =    getboard (empty ++ empty ++ "-Q------" ++ "-K------" ++ empty ++ "k-------" ++ empty ++ empty)
mateintwo =      getboard (empty ++ "-Q------" ++ empty ++ "k-------" ++ empty ++ "--K-----" ++ empty ++ empty)
initial =        getboard ("rnbqkbnr" ++ "pppppppp" ++ empty ++ empty ++ empty ++ empty ++ "PPPPPPPP" ++ "RNBQKBNR")
empty = "--------"

{-
DONE
TODO
  **Random bot?**
POSSIBLE TODO
ITS A REACH
  minimax
  AB pruning
CASTLING
  True
  king must be on 60
  rook must be on 63 or 56
  no pieces in the way
  can not move into check or move through check
  (technically cant have moves rooks or king)
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
showpiece (Piece Pawn True)    = 'P'
showpiece (Piece Knight True)  = 'N'
showpiece (Piece Bishop True)  = 'B'
showpiece (Piece Rook True)    = 'R'
showpiece (Piece Queen True)   = 'Q'
showpiece (Piece King True)    = 'K'
showpiece (Piece Pawn False)   = 'p'
showpiece (Piece Knight False) = 'n'
showpiece (Piece Bishop False) = 'b'
showpiece (Piece Rook False)   = 'r'
showpiece (Piece Queen False)  = 'q'
showpiece (Piece King False)   = 'k'

getpiece :: Char -> Maybe Piece
getpiece 'P' = Just (Piece Pawn True)
getpiece 'N' = Just (Piece Knight True)
getpiece 'B' = Just (Piece Bishop True)
getpiece 'R' = Just (Piece Rook True)
getpiece 'Q' = Just (Piece Queen True)
getpiece 'K' = Just (Piece King True)
getpiece 'p' = Just (Piece Pawn False)
getpiece 'n' = Just (Piece Knight False)
getpiece 'b' = Just (Piece Bishop False)
getpiece 'r' = Just (Piece Rook False)
getpiece 'q' = Just (Piece Queen False)
getpiece 'k' = Just (Piece King False)
getpiece  _  = Nothing




--input will look like a2c6
stringtomove :: String -> Move
stringtomove s | length s /= 4 = error ("error in stm: " ++ s)
stringtomove s = (ctr (s!!1) * 8 + ctc (s!!0), ctr (s!!3) * 8 + ctc (s!!2))

showspot :: Spot -> Char
showspot Nothing = '-'
showspot (Just piece) = showpiece piece

getboard :: String -> Board
getboard string = map getpiece string

showboard :: Board -> String
showboard b = map showspot b

setspotonboard :: Board -> Spot -> Int -> Board
setspotonboard (x:xs) a num
  | num == 0 = a:xs
  | otherwise = x:setspotonboard xs a (num-1)

getpieceonboard :: Board -> Int -> Spot
getpieceonboard (xs) i | i < 0 || i > 63 = error ("Out of bounds, getpieceonboard")
getpieceonboard (xs) i = xs !! i

executemove :: Board -> Move -> Board
executemove b (o,d) = setspotonboard (setspotonboard b (getpieceonboard b o) d) (getpiece '-') o

inrange :: Move -> Bool
inrange (o,d) = (o >= 0 && o <= 63) && (d >= 0 && d <= 63)

exists :: Board -> Int -> Bool
exists b i | i < 0 || i > 63 = error ("Out of bounds, exists function")
exists b i = (showspot (b !! i) /= '-')

notfriendlyfire :: Board -> Move -> Bool
notfriendlyfire b (o,d) = ((getcolor (getpieceonboard b o)) /= (getcolor (getpieceonboard b d)))

correctturn :: Board -> Int -> Bool -> Bool
correctturn b i _ | (i < 0 || i > 63) = error ("Out of bounds, correct turn")
correctturn b i True  = getcolor (b !! i) == True
correctturn b i False = getcolor (b !! i) == False

getcolor :: Spot -> Bool
getcolor (Just (Piece _ False)) = False
getcolor (Just (Piece _ True))  = True
getcolor _  = undefined

isenemy :: Board -> Int -> Bool -> Bool
isenemy b i t | i < 0 || i > 63 = error ("Out of bounds, isenemy function")
isenemy b i t = (exists b i) && ((getcolor (b !! i)) /= t)

--this function will handle moving a piece after it has been cleared
--this will help with castling, en passant, and promotion
premove :: Board -> Move -> Board
premove b (o,d) = b

getmoves :: Board -> Bool -> [Move]
getmoves b t = getmoves' b t 0 0

getmoves' :: Board -> Bool -> Int -> Int -> [Move]
getmoves' b t 63 63 = []
getmoves' b t i1 63 = if (validmove b (i1,63) t) then (i1,63):(getmoves' b t (i1 + 1) 0)  else (getmoves' b t (i1 + 1) 0)
getmoves' b t i1 i2 = if (validmove b (i1,i2) t) then (i1,i2):(getmoves' b t i1 (i2 + 1)) else (getmoves' b t i1 (i2 + 1))

validmove :: Board -> Move -> Bool -> Bool
validmove b (o,d) t = validmove'' b (o,d) t && (notcheckmove b (o,d) t)

validmove'' :: Board -> Move -> Bool -> Bool
validmove'' b (o,d) t = (inrange (o,d))
                   && o /= d
                   && (exists b o)
                   && (correctturn b o t)
                   && (if (exists b d) then (notfriendlyfire b (o,d)) else True)
                   && (validmove' b (o,d) (getpieceonboard b o))

validmove' :: Board -> Move -> Spot -> Bool
validmove' b m (Just (Piece Pawn True))    = pmovementw b m
validmove' b m (Just (Piece Pawn False))   = pmovementb b m
validmove' b m (Just (Piece Knight _))     = nmovement m
validmove' b m (Just (Piece Bishop _))     = (bmovement m) && (notblocked b m False)
validmove' b m (Just (Piece Rook _))       = (rmovement m) && (notblocked b m True)
validmove' b m (Just (Piece Queen _))      = ((rmovement m) && (notblocked b m True)) || ((bmovement m) && (notblocked b m False))
validmove' b m (Just (Piece King True))    = kmovement m || whitecastle b m
validmove' b m (Just (Piece King False))   = kmovement m || blackcastle b m
validmove' _ _ _                           = False

whitecastle :: Board -> Move -> Bool
whitecastle b (60, 62) = not (exists b 61) && not (exists b 62)
whitecastle b (60, 58) = not (exists b 57) && not (exists b 58) && not (exists b 59)
whitecastle b _        = False

blackcastle :: Board -> Move -> Bool
blackcastle b (4, 6) = not (exists b 5) && not (exists b 6)
blackcastle b (4, 1) = not (exists b 1) && not (exists b 2) && not (exists b 3)
blackcastle b _      = False

pmovementw :: Board -> Move -> Bool
pmovementw b (o,d) = ((col o == col d) && (row o - row d == 1) && (not (exists b d))) --if columns equal eachother and (destination row - origin row = 1) and no piece exists at destination
              || ((col o == col d) && (row o - row d == 2) && (not (exists b d)) && (not (exists b (o - 8))) && (row o == 6)) --this line is for first pawn move double move
              || ((o - d == 9) && (isenemy b d True) && (col o /= 0)) --upleft -9
              || ((o - d == 7) && (isenemy b d True) && (col o /= 7)) --upright -7

pmovementb :: Board -> Move -> Bool
pmovementb b (o,d) = ((col o == col d) && (row d - row o == 1) && (not (exists b d))) --one pawn move forward
              || ((col o == col d) && (row d - row o == 2) && (not (exists b d)) && (not (exists b (o + 8))) && (row o == 1)) --doublepawn move
              || ((d - o == 7) && (isenemy b d False) && (col o /= 0)) --downleft +7
              || ((d - o == 9) && (isenemy b d False) && (col o /= 7)) --downright +9


nmovement :: Move -> Bool
nmovement (o,d) = ((abs (row o - row d) * (abs (col o - col d))) == 2)

bmovement :: Move -> Bool
bmovement (o,d) = (abs(row o - row d) == abs(col o - col d))

kmovement :: Move -> Bool
kmovement (o,d) = (abs (row o - row d) <= 1 && (abs (col o - col d)) <= 1)

rmovement :: Move -> Bool
rmovement (o,d) = ((row o) == (row d)) /= (((col o) == (col d)))

--This bool is NOT side it is the type of movement True = Rook movement, False = Bishop Movement
notblocked :: Board -> Move -> Bool -> Bool
notblocked b (o,d) True  = if (col o == col d) then rookvertical b (o,d) else rookhorizontal b (o,d)
notblocked b (o,d) False = if (mod o 9 == mod d 9) then bishopleft b (o,d) else bishopright b (o,d)

rookhorizontal :: Board -> Move -> Bool
rookhorizontal b (o,d) = if (col o < col d) then rookright b (o,d) else rookleft b (o,d)

rookvertical :: Board -> Move -> Bool
rookvertical b (o,d) = if (row o < row d) then rookdown b (o,d) else rookup b (o,d)

rookup :: Board -> Move -> Bool
rookup b (o,d) = if ((not (inrange (o - 8,d))) || (exists b (o - 8) && (o - 8 /= d))) then False else (if (o - 8 == d) then True else rookup b (o - 8, d))

rookdown :: Board -> Move -> Bool
rookdown b (o,d) = if ((not (inrange (o + 8,d))) || (exists b (o + 8) && (o + 8 /= d))) then False else (if (o + 8 == d) then True else rookdown b (o + 8, d))

rookleft :: Board -> Move -> Bool
rookleft b (o,d) = if ((not (inrange (o - 1,d))) || (exists b (o - 1) && (o - 1 /= d))) then False else (if (o - 1 == d) then True else rookleft b (o - 1, d))

rookright :: Board -> Move -> Bool
rookright b (o,d) = if ((not (inrange (o + 1,d))) || (exists b (o + 1) && (o + 1 /= d))) then False else (if (o + 1 == d) then True else rookright b (o + 1, d))



bishopleft :: Board -> Move -> Bool
bishopleft b (o,d) = if (o < d) then downright b (o,d) else upleft b (o,d)

bishopright :: Board -> Move -> Bool
bishopright b (o,d) = if (o < d) then downleft b (o,d) else upright b (o,d)

downright :: Board -> Move -> Bool
downright b (o,d) = if ((not (inrange (o + 9,d))) || (exists b (o + 9) && (o + 9 /= d))) then False else (if (o + 9 == d) then True else downright b (o + 9, d))

downleft :: Board -> Move -> Bool
downleft b (o,d)  = if ((not (inrange (o + 7,d))) || (exists b (o + 7) && (o + 7 /= d))) then False else (if (o + 7 == d) then True else downleft b (o + 7, d))

upright :: Board -> Move -> Bool
upright b (o,d)   = if ((not (inrange (o - 7,d))) || (exists b (o - 7) && (o - 7 /= d))) then False else (if (o - 7 == d) then True else upright b (o - 7, d))

upleft :: Board -> Move -> Bool
upleft b (o,d)    = if ((not (inrange (o - 9,d))) || (exists b (o - 9) && (o - 9 /= d))) then False else (if (o - 9 == d) then True else upleft b (o - 9, d))


--LEFT DIAGONAL \ mod 9
--RIGHT DIAGONAL / mod 7

stalemate :: Board -> Bool -> Bool
stalemate b t = (null (getmoves b t)) && (not (incheck b True)) && (not (incheck b False))

checkmate :: Board -> Bool -> Bool
checkmate b t = (null (getmoves b t)) && (incheck b t)

--returns true if and only if the square is attacked by the side opposite of the boolean given
attacked :: Board -> Int -> Bool -> Bool
attacked b i t = attacked' b i t 0

attacked' :: Board -> Int -> Bool -> Int -> Bool
attacked' b i t 64 = False
attacked' b i t n  = if (validmove'' b (n,i) (not t)) then True else attacked' b i t (n+1)

--returns first instance of piece given
findpiece :: Board -> Spot -> Int
findpiece b p = findpiece' b p 0

findpiece' :: Board -> Spot -> Int -> Int
findpiece' b p n | n < 0 || n > 63 = -1
findpiece' b p n = if (b !! n == p) then n else findpiece' b p (n+1)

existskings :: Board -> Bool
existskings b = (findpiece b (Just (Piece King True)) /= -1) && (findpiece b (Just (Piece King False)) /= -1)

incheck :: Board -> Bool -> Bool
incheck b True  = attacked b (findpiece b (Just(Piece King True))) True
incheck b False = attacked b (findpiece b (Just(Piece King False))) False

notcheckmove :: Board -> Move -> Bool -> Bool
notcheckmove b (o,d) t | d < 0 || d > 63 = error ("error in notcheckmove" ++ boardtostring b)
notcheckmove b (o,d) t = not (incheck (executemove b (o,d)) t) && b!!d /= (Just (Piece King True)) && b!!d /= (Just (Piece King False)) && existskings b

staticeval :: Board -> Int -> Double
staticeval b d | checkmate b False = minVal + fromIntegral d
staticeval b d | checkmate b True  = maxVal - fromIntegral d
staticeval b d | stalemate b True || stalemate b False  = 0.0
staticeval b d = staticeval' b 0

staticeval' :: Board -> Int -> Double
staticeval' b 64 = 0
staticeval' b n  = (value piecec)  + (weightedposition piecec n) + (staticeval' b (n+1))
    where
        piecec = getpieceonboard b n

weightedposition :: Spot -> Int -> Double
weightedposition _ n | n < 0 || n > 63 = error ("error in weighted position")
weightedposition (Just (Piece Pawn True))    n   =      weightedpawnwhite   !! n
weightedposition (Just (Piece Knight True))  n   =      weightedknightwhite !! n
weightedposition (Just (Piece Bishop True))  n   =      weightedbishopwhite !! n
weightedposition (Just (Piece Rook True))    n   =      weightedrookwhite   !! n
weightedposition (Just (Piece Queen True))   n   =      weightedqueenwhite  !! n
weightedposition (Just (Piece King True))    n   =      weightedkingwhite   !! n
weightedposition (Just (Piece Pawn False))   n   = -1 * weightedpawnblack   !! n
weightedposition (Just (Piece Knight False)) n   = -1 * weightedknightblack !! n
weightedposition (Just (Piece Bishop False)) n   = -1 * weightedbishopblack !! n
weightedposition (Just (Piece Rook False))   n   = -1 * weightedrookblack   !! n
weightedposition (Just (Piece Queen False))  n   = -1 * weightedqueenblack  !! n
weightedposition (Just (Piece King False))   n   = -1 * weightedkingblack   !! n
weightedposition _ _ = 0.0



value :: Spot -> Double
value (Just (Piece Pawn True))       = 10.2
value (Just (Piece Pawn False))      = -10.2
value (Just (Piece Knight True))     = 30.5
value (Just (Piece Bishop True))     = 33.3
value (Just (Piece Rook True))       = 56.3
value (Just (Piece Queen True))      = 95.1
value (Just (Piece King True))       = 999.9
value (Just (Piece Knight False))    = -30.5
value (Just (Piece Bishop False))    = -33.3
value (Just (Piece Rook False))      = -56.3
value (Just (Piece Queen False))     = -95.1
value (Just (Piece King False))      = -999.9
value _                              = 0

boardtostring :: Board -> String
boardtostring b = "\n" ++ "==ABCDEFGH==" ++ "\n" ++ (unlines $ zipWith (++)(zipWith (++) ["8 ","7 ","6 ","5 ","4 ","3 ","2 ","1 "] (format $ (showboard b))) [" 8"," 7"," 6"," 5"," 4"," 3"," 2"," 1"] ) ++ "==ABCDEFGH==" ++ "\n"


getbotmove :: Board -> Bool -> Move
getbotmove b t = findbestmove b t


maxVal = -9876
minVal = 9876
maxdepth = 1

findbestmove :: Board -> Bool -> Move
findbestmove b True | null $ getmoves b True = error("no moves in findbestmove")
findbestmove b True = snd $ maximum y
    where
        moves = getmoves b True
        y = map (\x -> (minimax (executemove b x) (getmoves b True) True maxdepth, x)) moves
findbestmove b False | null $ getmoves b False = error("no moves in findbestmove")
findbestmove b False = snd $ minimum y
    where
        moves = getmoves b False
        y = map (\x -> (minimax (executemove b x) (getmoves b False) False maxdepth, x)) moves



minimax :: Board -> [Move] -> Bool -> Int -> Double
minimax b m ismaxing depth | depth == 0         = staticeval b depth
minimax b m t d | stalemate b t                 = 0.0
minimax b m t d | checkmate b True              = maxVal - fromIntegral d
minimax b m t d | checkmate b False             = minVal + fromIntegral d
minimax b [] _    depth                         = staticeval b depth
minimax b m True  depth | length m == 1         = (minimax (executemove b (head m)) (getmoves b False) False (depth-1))
minimax b m False depth | length m == 1         = (minimax (executemove b (head m)) (getmoves b True)  True  (depth-1))
minimax b (x:xs) True  depth                    = if (nextnode > 100) then nextnode else max nextnode (minimax b xs True  depth)
            where nextnode = (minimax (executemove b x) (getmoves b False) False (depth-1))
minimax b (x:xs) False depth                    = if (nextnode < 100) then nextnode else min nextnode (minimax b xs False depth)
            where nextnode = (minimax (executemove b x) (getmoves b True)  True  (depth-1))
