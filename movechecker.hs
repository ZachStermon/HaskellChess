module Movechecker
( getmoves
, getmoves'
, exists
, inrange
, checkmate
, stalemate
, validmove
, gameover
) where

import Types
import Chess
import Helpers

-- Returns a list of up to four potentially legal pawn moves
getpawnmoves :: State -> Position -> [Position]
getpawnmoves (State {board = b, turn = False}) p =
  let m1 = if null (b !! (p + 8)) then [p+8] else []
      m2 = if row p == 1 && not (null m1) && null (b !! (p + 16)) then (p+16):m1 else m1
      m3 = if (isenemy b (p+7) False) then (p+7):m2 else m2
      m4 = if (isenemy b (p+9) False) then (p+9):m3 else m3
  in m4
getpawnmoves (State {board = b, turn = True}) p =
  let m1 = if null (b !! (p-8)) then [p-8] else []
      m2 = if row p == 6 && not (null m1) && null (b !! (p-16)) then (p-16):m1 else m1
      m3 = if (isenemy b (p-7) True) then (p-7):m2 else m2
      m4 = if (isenemy b (p-9) True) then (p-9):m3 else m3
  in m4


--returns a list of potentially legal rook moves
getrookmoves :: State -> Position -> [Position]
getrookmoves (State {board = b, turn = t}) p =
  let checkup acc pos    | pos < 0              = acc
                         | isenemy b pos t      = pos:acc
                         | null (b!!pos)        = checkup (pos:acc) (pos-8)
                         | otherwise            = acc
      checkdown acc pos  | pos > 63             = acc
                         | isenemy b pos t      = pos:acc
                         | null (b!!pos)        = checkdown (pos:acc) (pos+8)
                         | otherwise            = acc
      checkleft acc pos  | (pos `mod` 8) == 7   = acc
                         | isenemy b pos t      = pos:acc
                         | null (b!!pos)        = checkleft (pos:acc) (pos-1)
                         | otherwise            = acc
      checkright acc pos | (pos `mod` 8) == 0   = acc
                         | isenemy b pos t      = pos:acc
                         | null (b!!pos)        = checkright (pos:acc) (pos+1)
                         | otherwise            = acc
  in checkup [] (p-8) ++ checkdown [] (p+8) ++ checkleft [] (p-1) ++ checkright [] (p+1)








-- under this line is bad but good for reference
--add function that gives list of moves for specific piece
getmoves :: Board -> Side -> [Move]
getmoves b t = getmovesr b t 0 0

getmovesr :: Board -> Side -> Position -> Position -> [Move]
getmovesr b t 63 63 = []
getmovesr b t i1 63 = if validmove b (i1,63) t then (i1,63):(getmovesr b t (i1 + 1) 0)  else getmovesr b t (i1 + 1) 0
getmovesr b t i1 i2 = if validmove b (i1,i2) t then (i1,i2):(getmovesr b t i1 (i2 + 1)) else getmovesr b t i1 (i2 + 1)

getmoves' :: Board -> Side -> [Move]
getmoves' b t = getmovesr' b t 0 0

getmovesr' :: Board -> Side -> Position -> Position -> [Move]
getmovesr' b t 63 63 = []
getmovesr' b t i1 63 = if validmove'' b (i1,63) t then (i1,63):(getmovesr' b t (i1 + 1) 0)  else getmovesr' b t (i1 + 1) 0
getmovesr' b t i1 i2 = if validmove'' b (i1,i2) t then (i1,i2):(getmovesr' b t i1 (i2 + 1)) else getmovesr' b t i1 (i2 + 1)

validmove :: Board -> Move -> Side -> Bool
validmove b (o,d) t = validmove'' b (o,d) t && notcheckmove b (o,d) t

validmove'' :: Board -> Move -> Side -> Bool
validmove'' b (o,d) t = (inrange (o,d))
                   && o /= d
                   && exists b o
                   && correctturn b o t
                   && (if (exists b d) then (notfriendlyfire b (o,d)) else True)
                   && validmove' b (o,d) (b!!o)

--TODO
validmove' :: Board -> Move -> Spot -> Bool
validmove' b m (Just (Piece Pawn True))  = pmovementw b m
validmove' b m (Just (Piece Pawn False)) = pmovementb b m
validmove' b m (Just (Piece Knight _))   = nmovement m
validmove' b m (Just (Piece Bishop _))   = bmovement m && notblocked b m False
validmove' b m (Just (Piece Rook _))     = rmovement m && notblocked b m True
validmove' b m (Just (Piece Queen _))    = (rmovement m && notblocked b m True) || (bmovement m && notblocked b m False)
validmove' b m (Just (Piece King True))  = kmovement m || whitecastle b m
validmove' b m (Just (Piece King False)) = kmovement m || blackcastle b m
validmove' _ _ _                         = False


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


--All of the movement functions and helpers pertaining to the Bishop
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



incheck :: Board -> Bool -> Bool
incheck b t  = attacked b (findpiece b (Just (Piece King t))) t


notcheckmove :: Board -> Move -> Bool -> Bool
notcheckmove b (o,d) t | d < 0 || d > 63 = error ("error in notcheckmove")
notcheckmove b (o,d) t = not (incheck (premove b (o,d)) t) && b!!d /= (Just (Piece King True)) && b!!d /= (Just (Piece King False))


gameover :: Board -> Bool -> Bool
gameover b t = (checkmate b t) || (stalemate b t)

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
