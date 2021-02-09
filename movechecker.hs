module Movechecker
( getmoves
, exists
, inrange
, validmove
, incheck
, stalemate
, checkmate
, notabletomove
, attacked
) where

--Necessary imports
import Types
import Chess
import Helpers

--FOR TESTING ONLY
import Boards



-- Returns a list of up to four potentially legal pawn moves
getpawnmoves :: State -> Position -> [Move]
getpawnmoves (State {board = b, turn = False}) p =
  let m1 = if null (b !! (p + 8)) then [(p,p+8)] else []
      m2 = if row p == 1 && null (b!!(p+8)) && null (b!!(p+16)) then (p,p+16):m1 else m1
      m3 = if (isenemy b (p+7) False) then (p,p+7):m2 else m2
      m4 = if (isenemy b (p+9) False) then (p,p+9):m3 else m3
  in m4
getpawnmoves (State {board = b, turn = True}) p =
  let m1 = if null (b !! (p-8)) then [(p,p-8)] else []
      m2 = if row p == 6 && null (b !! (p-8)) && null (b !! (p-16)) then (p,p-16):m1 else m1
      m3 = if (isenemy b (p-7) True) then (p,p-7):m2 else m2
      m4 = if (isenemy b (p-9) True) then (p,p-9):m3 else m3
  in m4


--returns a list of potentially legal rook moves
getrookmoves :: State -> Position -> [Move]
getrookmoves (State {board = b, turn = t}) p    =
  let checkup acc pos    | pos < 0              = acc
                         | isenemy b pos t      = (p,pos):acc
                         | null (b!!pos)        = checkup ((p,pos):acc) (pos-8)
                         | otherwise            = acc
      checkdown acc pos  | pos > 63             = acc
                         | isenemy b pos t      = (p,pos):acc
                         | null (b!!pos)        = checkdown ((p,pos):acc) (pos+8)
                         | otherwise            = acc
      checkleft acc pos  | (pos `mod` 8) == 7   = acc
                         | isenemy b pos t      = (p,pos):acc
                         | null (b!!pos)        = checkleft ((p,pos):acc) (pos-1)
                         | otherwise            = acc
      checkright acc pos | (pos `mod` 8) == 0   = acc
                         | isenemy b pos t      = (p,pos):acc
                         | null (b!!pos)        = checkright ((p,pos):acc) (pos+1)
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
                             | isenemy b pos t      = (p,pos):acc
                             | null (b!!pos)        = checkupleft ((p,pos):acc) (pos-9)
                             | otherwise            = acc
      checkdownleft acc pos  | not (bmovement (p,pos)) || not (inrange (p,pos))   = acc
                             | isenemy b pos t      = (p,pos):acc
                             | null (b!!pos)        = checkdownleft ((p,pos):acc) (pos+7)
                             | otherwise            = acc
      checkdownright acc pos | not (bmovement (p,pos)) || not (inrange (p,pos))   = acc
                             | isenemy b pos t      = (p,pos):acc
                             | null (b!!pos)        = checkdownright ((p,pos):acc) (pos+9)
                             | otherwise            = acc
      checkupright acc pos   | not (bmovement (p,pos)) || not (inrange (p,pos))   = acc
                             | isenemy b pos t      = (p,pos):acc
                             | null (b!!pos)        = checkupright ((p,pos):acc) (pos-7)
                             | otherwise            = acc
  in checkupleft [] (p-9) ++ checkdownleft [] (p+7) ++ checkdownright [] (p+9) ++ checkupright [] (p-7)

--makes sure the bishop does not wrongfully travel across the board
bmovement :: Move -> Bool
bmovement (o,d) = (abs(row o - row d) == abs(col o - col d))


--gets a list of all potentially valid king moves including castling
getkingmoves :: State -> Position -> [Move]
getkingmoves s p =
  let b = board s
      m1 = if inrange (p,p+1) && notfriendlyfire b (p,p+1) && kmovement (p,p+1) then [(p,p+1)]  else [] ++ getcastlemoves s p
      m2 = if inrange (p,p+9) && notfriendlyfire b (p,p+9) && kmovement (p,p+9) then (p,p+9):m1 else m1
      m3 = if inrange (p,p+8) && notfriendlyfire b (p,p+8) && kmovement (p,p+8) then (p,p+8):m2 else m2
      m4 = if inrange (p,p+7) && notfriendlyfire b (p,p+7) && kmovement (p,p+7) then (p,p+7):m3 else m3
      m5 = if inrange (p,p-1) && notfriendlyfire b (p,p-1) && kmovement (p,p-1) then (p,p-1):m4 else m4
      m6 = if inrange (p,p-9) && notfriendlyfire b (p,p-9) && kmovement (p,p-9) then (p,p-9):m5 else m5
      m7 = if inrange (p,p-8) && notfriendlyfire b (p,p-8) && kmovement (p,p-8) then (p,p-8):m6 else m6
      m8 = if inrange (p,p-7) && notfriendlyfire b (p,p-7) && kmovement (p,p-7) then (p,p-7):m7 else m7
  in m8

kmovement :: Move -> Bool
kmovement (o,d) = (abs (row o - row d) <= 1 && (abs (col o - col d)) <= 1)

getcastlemoves :: State -> Position -> [Move]
getcastlemoves (State {turn = True,  board = b, wl = z, ws = y}) p =
  let m1 = if z && whitecastle b (p, p+2) then [(p,p+2)]    else []
      m2 = if y && whitecastle b (p, p-2) then (p,p-2):m1   else m1
  in m2
getcastlemoves (State {turn = False, board = b, bl = z, bs = y}) p =
  let m1 = if z && blackcastle b (p, p+2) then [(p,p+2)]    else []
      m2 = if y && blackcastle b (p, p-2) then (p,p-2):m1   else m1
  in m2



getmoves :: State -> Position -> [Move]
getmoves s 64 = []
getmoves s n  =
  let b = board s
  in getmovesforspot s (b!!n) (turn s) n ++ getmoves s (n+1)

getmovesforspot :: State -> Spot -> Turn -> Position -> [Move]
getmovesforspot s Nothing t n                   = []
getmovesforspot s spot t n | getcolor spot /= t = []
getmovesforspot s (Just (Piece Pawn _))   t n   = getpawnmoves   s n
getmovesforspot s (Just (Piece Knight _)) t n   = getknightmoves s n
getmovesforspot s (Just (Piece Bishop _)) t n   = getbishopmoves s n
getmovesforspot s (Just (Piece Rook _))   t n   = getrookmoves   s n
getmovesforspot s (Just (Piece Queen _))  t n   = getbishopmoves s n ++ getrookmoves s n
getmovesforspot s (Just (Piece King _))   t n   = getkingmoves   s n


--TODO updated getmoves function
validmove :: State -> Move -> Bool
validmove s (o,d) = (o,d) `elem` (getmoves s 0)








--
incheck :: State -> Bool
incheck s  = attacked (updateturn s) (findpiece (board s) (Just (Piece King (turn s))))

--
-- notcheckmove :: Board -> Move -> Bool -> Bool
-- notcheckmove b (o,d) t | d < 0 || d > 63 = error ("error in notcheckmove")
-- notcheckmove b (o,d) t = not (incheck (premove b (o,d)) t) && b!!d /= (Just (Piece King True)) && b!!d /= (Just (Piece King False))
--
--
-- gameover :: Board -> Bool -> Bool
-- gameover b t = (checkmate b t) || (stalemate b t)
--

--not in check and not able to move
stalemate :: State -> Bool
stalemate s = not (incheck s) && notabletomove s (getmoves s 0)

--as soon as a valid move is seen (a move that does not put itself in check) False is returned else True
notabletomove :: State -> [Move] -> Bool
notabletomove s [] = True
notabletomove s (x:xs) = if incheck (updateturn (domove s x)) then notabletomove s xs else False

--in check and not able to move
checkmate :: State -> Bool
checkmate s = incheck s && notabletomove s (getmoves s 0)

--returns true if and only if the square is attacked by the current player
attacked :: State -> Int -> Bool
attacked s i = i `elem` (map snd (getmoves s 0))
