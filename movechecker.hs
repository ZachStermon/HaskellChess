module Movechecker
( getmoves
, validmove
, incheck
, stalemate
, checkmate
, notabletomove
, attacked
, gameover
, notcheckmove
) where

--Necessary imports
import Types
import Chess
import Helpers
import Data.Sequence
import Data.Maybe
import Data.Foldable (toList)

--FOR TESTING ONLY
import Boards
import Printing


-- Returns a list of up to four potentially legal pawn moves
getpawnmoves :: State -> Position -> [Move]
getpawnmoves (State {board = b, turn = False}) p =
  let m1 = if isNothing (index b (p + 8)) then [(p,p+8)] else []
      m2 = if row p == 1 && isNothing (index b (p+8)) && isNothing (index b (p+16)) then (p,p+16):m1 else m1
      m3 = if col p /= 0 && (isenemy b (p+7) False) then (p,p+7):m2 else m2
      m4 = if col p /= 0 && (isenemy b (p+9) False) then (p,p+9):m3 else m3
  in m4
getpawnmoves (State {board = b, turn = True}) p =
  let m1 = if isNothing (index b (p-8)) then [(p,p-8)] else []
      m2 = if row p == 6 && isNothing (index b (p-8)) && isNothing (index b (p-16)) then (p,p-16):m1 else m1
      m3 = if col p /= 7 && (isenemy b (p-7) True) then (p,p-7):m2 else m2
      m4 = if col p /= 7 && (isenemy b (p-9) True) then (p,p-9):m3 else m3
  in m4


--returns a list of potentially legal rook moves
getrookmoves :: State -> Position -> [Move]
getrookmoves (State {board = b, turn = t}) p    =
  let checkup acc pos    | pos < 0              = acc
                         | isenemy b pos t      = (p,pos):acc
                         | isNothing (index b pos) = checkup ((p,pos):acc) (pos-8)
                         | otherwise            = acc
      checkdown acc pos  | pos > 63             = acc
                         | isenemy b pos t      = (p,pos):acc
                         | isNothing (index b pos)   = checkdown ((p,pos):acc) (pos+8)
                         | otherwise            = acc
      checkleft acc pos  | (pos `mod` 8) == 7   = acc
                         | isenemy b pos t      = (p,pos):acc
                         | isNothing (index b pos)   = checkleft ((p,pos):acc) (pos-1)
                         | otherwise            = acc
      checkright acc pos | (pos `mod` 8) == 0   = acc
                         | isenemy b pos t      = (p,pos):acc
                         | isNothing (index b pos)   = checkright ((p,pos):acc) (pos+1)
                         | otherwise            = acc
  in checkup [] (p-8) ++ checkdown [] (p+8) ++ checkleft [] (p-1) ++ checkright [] (p+1)

--returns a list of potentially legal knight moves
getknightmoves :: State -> Position -> [Move]
getknightmoves (State {board = b, turn = t}) p =
  let m1 = if inrange (p,p+10) && notfriendlyfire b (p,p+10) && nmovement (p,p+10) then [(p,p+10)]  else []
      m2 = if inrange (p,p+17) && notfriendlyfire b (p,p+17) && nmovement (p,p+17) then (p,p+17):m1 else m1
      m3 = if inrange (p,p+15) && notfriendlyfire b (p,p+15) && nmovement (p,p+15) then (p,p+15):m2 else m2
      m4 = if inrange (p,p+6)  && notfriendlyfire b (p,p+6)  && nmovement (p,p+6)  then (p,p+6):m3  else m3
      m5 = if inrange (p,p-10) && notfriendlyfire b (p,p-10) && nmovement (p,p-10) then (p,p-10):m4 else m4
      m6 = if inrange (p,p-17) && notfriendlyfire b (p,p-17) && nmovement (p,p-17) then (p,p-17):m5 else m5
      m7 = if inrange (p,p-15) && notfriendlyfire b (p,p-15) && nmovement (p,p-15) then (p,p-15):m6 else m6
      m8 = if inrange (p,p-6)  && notfriendlyfire b (p,p-6)  && nmovement (p,p-6)  then (p,p-6):m7  else m7
  in m8

--checks the knight movement type so that it cannot travel accross the board
nmovement :: Move -> Bool
nmovement (o,d) = ((abs (row o - row d) * (abs (col o - col d))) == 2)

getbishopmoves :: State -> Position -> [Move]
getbishopmoves (State {board = b, turn = t}) p =
  let checkupleft acc pos    |  not (bmovement (p,pos)) || not (inrange (p,pos))  = acc
                             | isenemy b pos t                                    = (p,pos):acc
                             | isNothing (index b pos)                            = checkupleft ((p,pos):acc) (pos-9)
                             | otherwise                                          = acc
      checkdownleft acc pos  | not (bmovement (p,pos)) || not (inrange (p,pos))   = acc
                             | isenemy b pos t                                    = (p,pos):acc
                             | isNothing (index b pos)                            = checkdownleft ((p,pos):acc) (pos+7)
                             | otherwise                                          = acc
      checkdownright acc pos | not (bmovement (p,pos)) || not (inrange (p,pos))   = acc
                             | isenemy b pos t                                    = (p,pos):acc
                             | isNothing (index b pos)                            = checkdownright ((p,pos):acc) (pos+9)
                             | otherwise                                          = acc
      checkupright acc pos   | not (bmovement (p,pos)) || not (inrange (p,pos))   = acc
                             | isenemy b pos t                                    = (p,pos):acc
                             | isNothing (index b pos)                            = checkupright ((p,pos):acc) (pos-7)
                             | otherwise                                          = acc
  in checkupleft [] (p-9) ++ checkdownleft [] (p+7) ++ checkdownright [] (p+9) ++ checkupright [] (p-7)

--makes sure the bishop does not wrongfully travel across the board
bmovement :: Move -> Bool
bmovement (o,d) = (abs(row o - row d) == abs(col o - col d))


--gets a list of all potentially valid king moves including castling
getkingmoves :: State -> Position -> [Move]
getkingmoves s p =
  let b = board s
      m1 = if inrange (p,p+1) && notfriendlyfire b (p,p+1) && kmovement (p,p+1) then [(p,p+1)]  else []
      m2 = if inrange (p,p+9) && notfriendlyfire b (p,p+9) && kmovement (p,p+9) then (p,p+9):m1 else m1
      m3 = if inrange (p,p+8) && notfriendlyfire b (p,p+8) && kmovement (p,p+8) then (p,p+8):m2 else m2
      m4 = if inrange (p,p+7) && notfriendlyfire b (p,p+7) && kmovement (p,p+7) then (p,p+7):m3 else m3
      m5 = if inrange (p,p-1) && notfriendlyfire b (p,p-1) && kmovement (p,p-1) then (p,p-1):m4 else m4
      m6 = if inrange (p,p-9) && notfriendlyfire b (p,p-9) && kmovement (p,p-9) then (p,p-9):m5 else m5
      m7 = if inrange (p,p-8) && notfriendlyfire b (p,p-8) && kmovement (p,p-8) then (p,p-8):m6 else m6
      m8 = if inrange (p,p-7) && notfriendlyfire b (p,p-7) && kmovement (p,p-7) then (p,p-7):m7 else m7
  in m8

--validates king movement type
kmovement :: Move -> Bool
kmovement (o,d) = (abs (row o - row d) <= 1 && (abs (col o - col d)) <= 1)

--gets list of potentially valid moves by castling
getcastlemoves :: State -> Position -> [Move]
getcastlemoves s p | turn s =
  let m1 = if (wl s) && whitecastle s (p, p+2) then [(p,p+2)]    else []
      m2 = if (ws s) && whitecastle s (p, p-2) then (p,p-2):m1   else m1
  in m2
getcastlemoves s p =
  let m1 = if (bl s) && blackcastle s (p, p+2) then [(p,p+2)]    else []
      m2 = if (bs s) && blackcastle s (p, p-2) then (p,p-2):m1   else m1
  in m2


--gets a list of moves for the current board, pass in zero to test the entire board
--returns moves which might put the player in check
getmoves :: State -> [Move]
getmoves s | turn s    = concat $ map (\n -> getmovesforspot s (index (board s) n) n) (toList (whitepieces s))
           | otherwise = concat $ map (\n -> getmovesforspot s (index (board s) n) n) (toList (blackpieces s))


--returns a list of moves for a given spot at a specified position
getmovesforspot :: State -> Spot -> Position -> [Move]
getmovesforspot s (Just (Piece Pawn _))   n   = getpawnmoves   s n
getmovesforspot s (Just (Piece Knight _)) n   = getknightmoves s n
getmovesforspot s (Just (Piece Bishop _)) n   = getbishopmoves s n
getmovesforspot s (Just (Piece Rook _))   n   = getrookmoves   s n
getmovesforspot s (Just (Piece Queen _))  n   = getbishopmoves s n ++ getrookmoves   s n
getmovesforspot s (Just (Piece King _))   n   = getkingmoves   s n ++ getcastlemoves s n




--TODO updated getmoves function
validmove :: State -> Move -> Bool
validmove s (o,d) = (o,d) `elem` getmoves s && notcheckmove s (o,d)

notcheckmove :: State -> Move -> Bool
notcheckmove s m = not $ incheck (updateturn (domove s m))

--returns true if and only if the current sides king can be attacked by an enemy piece
incheck :: State -> Bool
incheck s  = attacked (updateturn s) (findpiece (board s) (Just (Piece King (turn s))))

--returns true if the current player is in checkmate or stalemate
gameover :: State -> Bool
gameover s = (checkmate s) || (stalemate s)


--not in check and not able to move
stalemate :: State -> Bool
stalemate s = not (incheck s) && notabletomove s (getmoves s)

--as soon as a valid move is seen (a move that does not put itself in check) False is returned else True
notabletomove :: State -> [Move] -> Bool
notabletomove s [] = True
notabletomove s (x:xs) = if incheck (updateturn (domove s x)) then notabletomove s xs else False

--in check and not able to move
checkmate :: State -> Bool
checkmate s = incheck s && notabletomove s (getmoves s)

--returns true if and only if the square is attacked by the current player
attacked :: State -> Maybe Int -> Bool
attacked s Nothing  = False
attacked s (Just i) = i `elem` (map snd (getmoves s))
