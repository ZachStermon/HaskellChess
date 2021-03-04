module Chess where

--Imports
import Helpers
import Types
import Printing
import Data.Maybe
import Data.Word
import Data.Bits

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




-- getbitboard :: BitBoard -> Int -> Word64
-- getbitboard b a | ((shift 1 a).&.(whitepawns b)) > 0 = whitepawns b
--                 | ((shift 1 a).&.(blackpawns b)) > 0 = blackpawns b
--                 | ((shift 1 a).&.(whiterooks b)) > 0 = whiterooks b
--                 | ((shift 1 a).&.(blackrooks b)) > 0 = blackrooks b
--
-- setmove :: BitBoard -> Move -> Word64
-- setmove bb (o,d) = (((getbitboard bb o) `clearBit` o) `setBit` d)


-- clearspot :: BitBoard -> Position -> BitBoard
-- clearspot bb p = bb { whitepawns   = (whitepawns bb)   `clearBit` n,
--                       blackpawns   = (blackpawns bb)   `clearBit` n,
--                       whiteknights = (whiteknights bb) `clearBit` n,
--                       blackknights = (blackknights bb) `clearBit` n,
--                       whitebishops = (whitebishops bb) `clearBit` n,
--                       blackbishops = (blackbishops bb) `clearBit` n,
--                       whiterooks   = (whiterooks bb)   `clearBit` n,
--                       blackrooks   = (blackrooks bb)   `clearBit` n,
--                       whitequeens  = (whitequeens bb)  `clearBit` n,
--                       blackqueens  = (blackqueens bb)  `clearBit` n,
--                       whitekings   = (whitekings bb)   `clearBit` n,
--                       blackkings   = (blackkings bb)   `clearBit` n}
--                           where n = fromEnum p
--
-- setspot :: BitBoard -> Piece -> Position -> BitBoard
-- setspot bb Void   n = clearspot bb n
-- setspot bb p      n = changevariable (clearspot bb n) p n

exists :: BitBoard -> Position -> Bool
exists bb p = testBit (occupied bb) (fromIntegral p)

empty :: BitBoard -> Position -> Bool
empty bb p = not $ exists bb p

rawmove :: Move -> Word64 -> Word64
rawmove (o,d) w = (w `clearBit` (fromIntegral o)) `setBit` (fromIntegral d)

--execute move takes a board and a move and will return a new board with the updated move having been played.
executemove :: BitBoard -> Move -> BitBoard
executemove bb (o,d) = b{whitepieces = getwhitepieces b, blackpieces = getblackpieces b, occupied = getoccupied b} where b = movepiece (removepiece bb d) (o,d)

movepiece :: BitBoard -> Move -> BitBoard
movepiece b@(BitBoard{whitepawns = wp})    (o,d) | testBit wp (fromIntegral o) = b{whitepawns   = rawmove (o,d) wp}
movepiece b@(BitBoard{whiteknights = wp})  (o,d) | testBit wp (fromIntegral o) = b{whiteknights = rawmove (o,d) wp}
movepiece b@(BitBoard{whitebishops = wp})  (o,d) | testBit wp (fromIntegral o) = b{whitebishops = rawmove (o,d) wp}
movepiece b@(BitBoard{whiterooks = wp})    (o,d) | testBit wp (fromIntegral o) = b{whiterooks   = rawmove (o,d) wp}
movepiece b@(BitBoard{whitequeens = wp})   (o,d) | testBit wp (fromIntegral o) = b{whitequeens  = rawmove (o,d) wp}
movepiece b@(BitBoard{whitekings = wp})    (o,d) | testBit wp (fromIntegral o) = b{whitekings   = rawmove (o,d) wp}
movepiece b@(BitBoard{blackpawns = wp})    (o,d) | testBit wp (fromIntegral o) = b{blackpawns   = rawmove (o,d) wp}
movepiece b@(BitBoard{blackknights = wp})  (o,d) | testBit wp (fromIntegral o) = b{blackknights = rawmove (o,d) wp}
movepiece b@(BitBoard{blackbishops = wp})  (o,d) | testBit wp (fromIntegral o) = b{blackbishops = rawmove (o,d) wp}
movepiece b@(BitBoard{blackrooks = wp})    (o,d) | testBit wp (fromIntegral o) = b{blackrooks   = rawmove (o,d) wp}
movepiece b@(BitBoard{blackqueens = wp})   (o,d) | testBit wp (fromIntegral o) = b{blackqueens  = rawmove (o,d) wp}
movepiece b@(BitBoard{blackkings = wp})    (o,d) | testBit wp (fromIntegral o) = b{blackkings   = rawmove (o,d) wp}
movepiece bb  _ = error("bit not set" ++ show bb)

removepiece :: BitBoard -> Position -> BitBoard
removepiece b@(BitBoard{blackpawns = bp})   p | testBit bp n = b{blackpawns   = bp `clearBit` n} where n = fromIntegral p
removepiece b@(BitBoard{blackknights = bp}) p | testBit bp n = b{blackknights = bp `clearBit` n} where n = fromIntegral p
removepiece b@(BitBoard{blackbishops = bp}) p | testBit bp n = b{blackbishops = bp `clearBit` n} where n = fromIntegral p
removepiece b@(BitBoard{blackrooks = bp})   p | testBit bp n = b{blackrooks   = bp `clearBit` n} where n = fromIntegral p
removepiece b@(BitBoard{blackqueens = bp})  p | testBit bp n = b{blackqueens  = bp `clearBit` n} where n = fromIntegral p
removepiece b@(BitBoard{blackkings = bp})   p | testBit bp n = b{blackkings   = bp `clearBit` n} where n = fromIntegral p
removepiece b@(BitBoard{whitepawns = bp})   p | testBit bp n = b{whitepawns   = bp `clearBit` n} where n = fromIntegral p
removepiece b@(BitBoard{whiteknights = bp}) p | testBit bp n = b{whiteknights = bp `clearBit` n} where n = fromIntegral p
removepiece b@(BitBoard{whitebishops = bp}) p | testBit bp n = b{whitebishops = bp `clearBit` n} where n = fromIntegral p
removepiece b@(BitBoard{whiterooks = bp})   p | testBit bp n = b{whiterooks   = bp `clearBit` n} where n = fromIntegral p
removepiece b@(BitBoard{whitequeens = bp})  p | testBit bp n = b{whitequeens  = bp `clearBit` n} where n = fromIntegral p
removepiece b@(BitBoard{whitekings = bp})   p | testBit bp n = b{whitekings   = bp `clearBit` n} where n = fromIntegral p
removepiece b _ = b












-- executemove' :: Side -> Move -> BitBoard -> BitBoard
-- executemove' t (o,d) bb = clearspot (setspot bb (getspot bb o) d) o

func :: BitBoard -> BitBoard
func bb = executemove (executemove bb (0,1)) (1,0)



func' :: Word8 -> Word8
func' w = toEnum $ fromIntegral w




--gets the color of a piece and returns either True for white or False for black.
getcolor :: Piece -> Bool
getcolor p = fromEnum p > 6


--Intermediary between states and premove
dmove :: State -> Move -> State
dmove s m = updatecastle (updateturn (updatehistory (premove s m) m)) m


--update board will update the state board when the board changes.
updateboard :: State -> BitBoard -> State
updateboard state bb = state {board = bb}

--updates the history of the current state.
updatehistory :: State -> Move -> State
updatehistory state m = state {history = (history state) ++ [m]}

--updates the current turn of the state.
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

--tests for if the said move is in range of the board, within 0 and 63
inrange :: Move -> Bool
inrange (o,d) = (o >= 0 && o <= 63) && (d >= 0 && d <= 63)




--makes sure that the piece being attacked is not the same color, if so notfriendlyfire returns True
notfriendlyfire :: BitBoard -> Move -> Bool
notfriendlyfire bb (o,d) = empty bb d || ((getcolor $ getspot bb o) /= (getcolor $ getspot bb d))





-- this function will handle moving a piece after it has been cleared
-- this will help with castling, en passant, and promote
-- also takes a state object so that it can determine if castling can be done
-- TODO
premove :: State -> Move -> State
premove s (o,d)  | (getspot (board s) o) == WhitePawn = if (row o == 1) then promote s (o,d)  else updateboard s (executemove (board s) (o,d))
                 | (getspot (board s) o) == BlackPawn = if (row o == 6) then promote s (o,d) else updateboard s (executemove (board s) (o,d))
                 | o == 60 || o == 4                  = if whitecastle s (o,d) || blackcastle s (o,d) then docastle s (o,d) else updateboard s (executemove (board s) (o,d))
premove s m                                           = updateboard s (executemove (board s) m)


--This function handles the promoting of pawns into queens, updates the board with a new queen piece.
promote :: State -> Move -> State
promote s (o,d) = updateboard s (changevariable (changevariable (board s) Void o) (if turn s then WhiteQueen else BlackQueen) d)

--the driver for castling, actually performs the moves to move the pieces.
docastle :: State -> Move -> State
docastle s (60, 58) = updateboard s (executemove (executemove (board s) (56,59)) (60, 58))
docastle s (60, 62) = updateboard s (executemove (executemove (board s) (63,61)) (60, 62))
docastle s (4,6)    = updateboard s (executemove (executemove (board s) (7,5)) (4,6))
docastle s (4,2)    = updateboard s (executemove (executemove (board s) (0,3)) (4,2))

--checks for castling on white team.
whitecastle :: State -> Move -> Bool
whitecastle s (60, 62) = empty (board s) 61 && empty (board s) 62
whitecastle s (60, 58) = empty (board s) 57 && empty (board s) 58 && empty (board s) 59
whitecastle s _        = False


--checks for castling on black team.
blackcastle :: State -> Move -> Bool
blackcastle s (4, 6) = empty (board s) 5 && empty (board s) 6
blackcastle s (4, 2) = empty (board s) 1 && empty (board s) 2 && empty (board s) 3
blackcastle s _      = False




--checks the opposite of notfriendlyfire, makes sure that piece being attacked is opposite color.
isenemy :: BitBoard -> Position -> Side -> Bool
isenemy bb i t = exists bb i && (getcolor $ getspot bb i) /= t




moveisanattack :: State -> Move -> Bool
moveisanattack s (o,d) = exists (board s) d






--comment
