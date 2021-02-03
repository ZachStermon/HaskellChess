module Chess
( premove
, whitecastle
, blackcastle
, findpiece
, notfriendlyfire
, correctturn
, inrange
, exists
, isenemy
) where

import Helpers
import Types
import Printing

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

--



setspotonboard :: Board -> Spot -> Position -> Board
setspotonboard (x:xs) a num
  | num == 0 = a:xs
  | otherwise = x:setspotonboard xs a (num-1)

executemove :: Board -> Move -> Board
executemove b (o,d) = setspotonboard (setspotonboard b (b!!o) d) Nothing o

getcolor :: Spot -> Bool
getcolor (Just (Piece _ t)) = t
getcolor _  = undefined



makestate :: Board -> Turn -> State
makestate b t = State {
  board = b,
  turn = t,
  history = [],
  blackcanlongcastle = True,
  blackcanshortcastle = True,
  whitecanlongcastle = True,
  whitecanshortcastle = True,
  capturedpieces = [] }








inrange :: Move -> Bool
inrange (o,d) = (o >= 0 && o <= 63) && (d >= 0 && d <= 63)

exists :: Board -> Position -> Bool
exists b i | i < 0 || i > 63 = error ("Out of bounds, exists function")
exists b i = (b !! i) /= Nothing

notfriendlyfire :: Board -> Move -> Bool
notfriendlyfire b (o,d) = (getcolor $ b!!o) /= (getcolor $ b!!d)

correctturn :: Board -> Int -> Side -> Bool
correctturn b i _ | (i < 0 || i > 63) = error ("Out of bounds, correct turn")
correctturn b i t  = getcolor (b!!i) == t



isenemy :: Board -> Position -> Side -> Bool
isenemy b i t | i < 0 || i > 63 = error ("Out of bounds, isenemy function")
isenemy b i t = (exists b i) && (getcolor $ b !! i) /= t

--this function will handle moving a piece after it has been cleared
--this will help with castling, en passant, and promote
--TODO
premove :: Board -> Move -> Board
premove b (o,d) | b!!o == (Just (Piece Pawn True))  = if (row o == 1) then promote b (o,d) True  else executemove b (o,d)
premove b (o,d) | b!!o == (Just (Piece Pawn False)) = if (row o == 6) then promote b (o,d) False else executemove b (o,d)
premove b (o,d) | o == 60 || o == 4                 = if (whitecastle b (o,d) || blackcastle b (o,d)) then docastle b (o,d) else executemove b (o,d)
premove b m                                         = executemove b m

promote :: Board -> Move -> Side -> Board
promote b (o,d) t = setspotonboard (setspotonboard b Nothing o) (Just (Piece Queen t)) d


docastle :: Board -> Move -> Board
docastle b (60, 58) = executemove (executemove b (56,59)) (60, 58)
docastle b (60, 62) = executemove (executemove b (63,61)) (60, 62)
docastle b (4,6)    = executemove (executemove b (7,5)) (4,6)
docastle b (4,2)    = executemove (executemove b (0,3)) (4,2)


whitecastle :: Board -> Move -> Bool
whitecastle b (60, 62) = not (exists b 61) && not (exists b 62)
whitecastle b (60, 58) = not (exists b 57) && not (exists b 58) && not (exists b 59)
whitecastle b _        = False

blackcastle :: Board -> Move -> Bool
blackcastle b (4, 6) = not (exists b 5) && not (exists b 6)
blackcastle b (4, 2) = not (exists b 1) && not (exists b 2) && not (exists b 3)
blackcastle b _      = False


--LEFT DIAGONAL \ mod 9
--RIGHT DIAGONAL / mod 7


--returns first instance of piece given
findpiece :: Board -> Spot -> Int
findpiece b p = findpiece' b p 0

findpiece' :: Board -> Spot -> Int -> Int
findpiece' b p n | n < 0 || n > 63 = -1
findpiece' b p n = if (b !! n == p) then n else findpiece' b p (n+1)




-- ?? TODO?
existskings :: Board -> Bool
existskings b = (findpiece b (Just (Piece King False)) /= -1) && (findpiece b (Just (Piece King True)) /= -1)
