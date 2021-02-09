module Chess
( domove
, whitecastle
, blackcastle
, notfriendlyfire
, correctturn
, inrange
, exists
, isenemy
, makestate
, getcolor
, premove
, findpiece
, existsblackking
, existswhiteking
, updateturn
) where

--Imports
import Helpers
import Types
import Printing

--FOR TESTING ONLY
import Boards

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
  wl = True,
  ws = True,
  bl = True,
  bs = True}

--Intermediary between states and premove
domove :: State -> Move -> State
domove s m = updatecastle (updateturn (updatehistory (premove s m) m)) m

updateboard :: State -> Board -> State
updateboard state b = state {board = b}

updatehistory :: State -> Move -> State
updatehistory state m = state {history = (history state) ++ [m]}

updateturn :: State -> State
updateturn state = state {turn = not (turn state)}

--TODO probably a better way to do this
updatecastle :: State -> Move -> State
updatecastle state (0,d)  = state {bl = False}
updatecastle state (7,d)  = state {bs = False}
updatecastle state (4,d)  = state {bs = False, bl = False}
updatecastle state (56,d) = state {wl = False}
updatecastle state (63,d) = state {ws = False}
updatecastle state (60,d) = state {ws = False, wl = False}
updatecastle s m = s


inrange :: Move -> Bool
inrange (o,d) = (o >= 0 && o <= 63) && (d >= 0 && d <= 63)

exists :: Board -> Position -> Bool
exists b i | i < 0 || i > 63 = error ("Out of bounds, exists function")
exists b i = (b !! i) /= Nothing

notfriendlyfire :: Board -> Move -> Bool
notfriendlyfire b (o,d) = null (b!!d) || ((getcolor $ b!!o) /= (getcolor $ b!!d))

correctturn :: Board -> Int -> Side -> Bool
correctturn b i _ | (i < 0 || i > 63) = error ("Out of bounds, correct turn")
correctturn b i t  = getcolor (b!!i) == t



isenemy :: Board -> Position -> Side -> Bool
isenemy b i t | i < 0 || i > 63 = error ("Out of bounds, isenemy function")
isenemy b i t = (exists b i) && (getcolor $ b !! i) /= t

-- this function will handle moving a piece after it has been cleared
-- this will help with castling, en passant, and promote
-- also takes a state object so that it can determine if castling can be done
-- TODO
premove :: State -> Move -> State
premove s (o,d)  | (board s)!!o == (Just (Piece Pawn True))  = if (row o == 1) then promote s (o,d)  else updateboard s (executemove (board s) (o,d))
                 | (board s)!!o == (Just (Piece Pawn False)) = if (row o == 6) then promote s (o,d) else updateboard s (executemove (board s) (o,d))
                 | o == 60 || o == 4                         = if whitecastle s (o,d) || blackcastle s (o,d) then docastle s (o,d) else updateboard s (executemove (board s) (o,d))
premove s m                                                  = updateboard s (executemove (board s) m)

promote :: State -> Move -> State
promote s (o,d) = updateboard s b
      where b = setspotonboard (setspotonboard (board s) Nothing o) (Just (Piece Queen (turn s))) d


docastle :: State -> Move -> State
docastle s (60, 58) = updateboard s (executemove (executemove (board s) (56,59)) (60, 58))
docastle s (60, 62) = updateboard s (executemove (executemove (board s) (63,61)) (60, 62))
docastle s (4,6)    = updateboard s (executemove (executemove (board s) (7,5)) (4,6))
docastle s (4,2)    = updateboard s (executemove (executemove (board s) (0,3)) (4,2))


whitecastle :: State -> Move -> Bool
whitecastle s (60, 62) = not (exists (board s) 61) && not (exists (board s) 62)
whitecastle s (60, 58) = not (exists (board s) 57) && not (exists (board s) 58) && not (exists (board s) 59)
whitecastle s _        = False

blackcastle :: State -> Move -> Bool
blackcastle s (4, 6) = not (exists (board s) 5) && not (exists (board s) 6)
blackcastle s (4, 2) = not (exists (board s) 1) && not (exists (board s) 2) && not (exists (board s) 3)
blackcastle s _      = False


--LEFT DIAGONAL \ mod 9
--RIGHT DIAGONAL / mod 7


--returns first instance of piece given
findpiece :: Board -> Spot -> Int
findpiece b p = findpiece' b p 0

findpiece' :: Board -> Spot -> Int -> Int
findpiece' b p n | n < 0 || n > 63 = -1
findpiece' b p n = if (b !! n == p) then n else findpiece' b p (n+1)




-- ?? TODO?
existsblackking :: Board -> Bool
existsblackking b = findpiece b (Just (Piece King False)) /= -1

existswhiteking :: Board -> Bool
existswhiteking b = findpiece b (Just (Piece King True)) /= -1
