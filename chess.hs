module Chess
( domove
, whitecastle
, blackcastle
, notfriendlyfire
, inrange
, makestate
, getcolor
, premove
, findpiece
, updateturn
, isenemy
) where

--Imports
import Helpers
import Types
import Printing
import Data.Sequence
import Data.Maybe

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




--this function sets a specified board position to input
setspotonboard :: Board -> Spot -> Position -> Board
setspotonboard b sp num = update num sp b


--execute move takes a board and a move and will return a new board with the updated move having been played.
executemove :: Board -> Move -> Board
executemove b (o,d) = setspotonboard (setspotonboard b (index b o) d) Nothing o

--gets the color of a piece and returns either True for white or False for black.
getcolor :: Spot -> Bool
getcolor (Just (Piece _ t)) = t
getcolor _  = undefined


--this is a helper function that makes a state out of all of the specified information and stores it for function use.
makestate :: Board -> Turn -> State
makestate b t = State {
  board = b,
  turn = t,
  history = [],
  whitepieces = findpieces b True,
  blackpieces = findpieces b False,
  wl = True,
  ws = True,
  bl = True,
  bs = True}

--Intermediary between states and premove
domove :: State -> Move -> State
domove s m = updatepieces (updatecastle (updateturn (updatehistory (premove s m) m)) m) m


--update board will update the state board when the board changes.
updateboard :: State -> Board -> State
updateboard state b = state {board = b}

--updates the history of the current state.
updatehistory :: State -> Move -> State
updatehistory state m = state {history = (history state) ++ [m]}

--updates the current turn of the state.
updateturn :: State -> State
updateturn state = state {turn = not (turn state)}

updatepieces :: State -> Move -> State
updatepieces s (o,d) | turn s  = s {whitepieces = findpieces (board s) True}
updatepieces s (o,d)           = s {blackpieces = findpieces (board s) False}

--TODO probably a better way to do this
updatecastle :: State -> Move -> State
updatecastle state (0,d)  = state {bl = False}
updatecastle state (7,d)  = state {bs = False}
updatecastle state (4,d)  = state {bs = False, bl = False}
updatecastle state (56,d) = state {wl = False}
updatecastle state (63,d) = state {ws = False}
updatecastle state (60,d) = state {ws = False, wl = False}
updatecastle s m = s

--tests for if the said move is in range of the board, within 0 and 63
inrange :: Move -> Bool
inrange (o,d) = (o >= 0 && o <= 63) && (d >= 0 && d <= 63)




--makes sure that the piece being attacked is not the same color, if so notfriendlyfire returns True
notfriendlyfire :: Board -> Move -> Bool
notfriendlyfire b (o,d) = isNothing (index b d) || ((getcolor $ index b o) /= (getcolor $ index b d))





-- this function will handle moving a piece after it has been cleared
-- this will help with castling, en passant, and promote
-- also takes a state object so that it can determine if castling can be done
-- TODO
premove :: State -> Move -> State
premove s (o,d)  | (index (board s) o) == (Just (Piece Pawn True))  = if (row o == 1) then promote s (o,d)  else updateboard s (executemove (board s) (o,d))
                 | (index (board s) o) == (Just (Piece Pawn False)) = if (row o == 6) then promote s (o,d) else updateboard s (executemove (board s) (o,d))
                 | o == 60 || o == 4                         = if whitecastle s (o,d) || blackcastle s (o,d) then docastle s (o,d) else updateboard s (executemove (board s) (o,d))
premove s m                                                  = updateboard s (executemove (board s) m)


--This function handles the promoting of pawns into queens, updates the board with a new queen piece.
promote :: State -> Move -> State
promote s (o,d) = updateboard s b
      where b = setspotonboard (setspotonboard (board s) Nothing o) (Just (Piece Queen (turn s))) d

--the driver for castling, actually performs the moves to move the pieces.
docastle :: State -> Move -> State
docastle s (60, 58) = updateboard s (executemove (executemove (board s) (56,59)) (60, 58))
docastle s (60, 62) = updateboard s (executemove (executemove (board s) (63,61)) (60, 62))
docastle s (4,6)    = updateboard s (executemove (executemove (board s) (7,5)) (4,6))
docastle s (4,2)    = updateboard s (executemove (executemove (board s) (0,3)) (4,2))

--checks for castling on white team.
whitecastle :: State -> Move -> Bool
whitecastle s (60, 62) = isNothing (index (board s) 61) && isNothing (index (board s) 62)
whitecastle s (60, 58) = isNothing (index (board s) 57) && isNothing (index (board s) 58) && isNothing (index (board s) 59)
whitecastle s _        = False


--checks for castling on black team.
blackcastle :: State -> Move -> Bool
blackcastle s (4, 6) = isNothing (index (board s) 5) && isNothing (index (board s) 6)
blackcastle s (4, 2) = isNothing (index (board s) 1) && isNothing (index (board s) 2) && isNothing (index (board s) 3)
blackcastle s _      = False


--LEFT DIAGONAL \ mod 9
--RIGHT DIAGONAL / mod 7


--returns first instance of piece given
findpiece :: Board -> Spot -> Maybe Int
findpiece b sp = elemIndexL sp b


--checks the opposite of notfriendlyfire, makes sure that piece being attacked is opposite color.
isenemy :: Board -> Position -> Side -> Bool
isenemy b i t = isJust (index b i) && (getcolor $ index b i) /= t


-- ?? TODO?
existsblackking :: Board -> Bool
existsblackking b = isJust $ findpiece b (Just (Piece King False))

existswhiteking :: Board -> Bool
existswhiteking b = isJust $ findpiece b (Just (Piece King True))






-- returns list of positions that have a piece for a given side, should only be called at init
findpieces :: Board -> Turn -> [Position]
findpieces b t = findIndicesR (getcolor) b







--comment
