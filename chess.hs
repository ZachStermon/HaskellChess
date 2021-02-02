module Chess
( executemove
, Board
, Move
, gameover
, validmove
, stringtomove
, boardtostring
, findbestmove
, initial
, premove
) where

import Helpers
import Weights

--declarations
data Type = Pawn | Knight | Bishop | Rook | Queen | King deriving (Show, Eq)
data Piece = Piece Type Bool deriving (Show, Eq)
type Spot = Maybe Piece
type Board = [Spot]
type Move = (Position, Position)
type Position = Int
type Side = Bool

maxdepth = 0
maxmatedepth = 3
maxval = -9876
minval = 9876

staleboard =     getboard ("kr--------r-----------------------------r-------P-------K-------")
checkmateboard = getboard ("----k------------------------------q---------------PPb-----QKB--")
testboard =      getboard ("rnbqkb-rppp---pp-----------nNp-Q--B-------------PPPP-PPPRNB-K--R")
mateinthree =    getboard ("---------------kp------p----B-p----P-pQ--------PP-----PK-----q--")
mateinthreev2 =  getboard ("---rk--rp----pppb-p-------b-----Nq--PPn--P------P-P-N-PPR-B-QK-R") --best moves are as follows: (b4e1 (33,60)),(e1f1 (60,61)),(d8d1 (3,59))
mateintwo =      getboard ("---------Q--------------k-----------------K---------------------")
mateintwov2 =    getboard ("------k------ppp----b---p-q----------K------P---r-----PP---Q---R") --best moves are as follows: (c5f5 (26,29)),(f5f2 (29,53))
initial =        getboard ("rnbqkbnr" ++ "pppppppp" ++ empty ++ empty ++ empty ++ empty ++ "PPPPPPPP" ++ "RNBQKBNR")
empty = "--------"

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

showspot :: Spot -> Char
showspot (Just (Piece Pawn True))    = 'P'
showspot (Just (Piece Knight True))  = 'N'
showspot (Just (Piece Bishop True))  = 'B'
showspot (Just (Piece Rook True))    = 'R'
showspot (Just (Piece Queen True))   = 'Q'
showspot (Just (Piece King True))    = 'K'
showspot (Just (Piece Pawn False))   = 'p'
showspot (Just (Piece Knight False)) = 'n'
showspot (Just (Piece Bishop False)) = 'b'
showspot (Just (Piece Rook False))   = 'r'
showspot (Just (Piece Queen False))  = 'q'
showspot (Just (Piece King False))   = 'k'
showspot _                           = '-'

getspot :: Char -> Spot
getspot 'P' = Just (Piece Pawn True)
getspot 'N' = Just (Piece Knight True)
getspot 'B' = Just (Piece Bishop True)
getspot 'R' = Just (Piece Rook True)
getspot 'Q' = Just (Piece Queen True)
getspot 'K' = Just (Piece King True)
getspot 'p' = Just (Piece Pawn False)
getspot 'n' = Just (Piece Knight False)
getspot 'b' = Just (Piece Bishop False)
getspot 'r' = Just (Piece Rook False)
getspot 'q' = Just (Piece Queen False)
getspot 'k' = Just (Piece King False)
getspot  _  = Nothing


getboard :: String -> Board
getboard string = map getspot string

showboard :: Board -> String
showboard b = map showspot b


setspotonboard :: Board -> Spot -> Position -> Board
setspotonboard (x:xs) a num
  | num == 0 = a:xs
  | otherwise = x:setspotonboard xs a (num-1)

executemove :: Board -> Move -> Board
executemove b (o,d) = setspotonboard (setspotonboard b (b!!o) d) Nothing o

getcolor :: Spot -> Bool
getcolor (Just (Piece _ t)) = t
getcolor _  = undefined


boardtostring :: Board -> String
boardtostring b = "\n" ++ "==ABCDEFGH==" ++ "\n" ++ (unlines $ zipWith (++)(zipWith (++) ["8 ","7 ","6 ","5 ","4 ","3 ","2 ","1 "] (format $ (showboard b))) [" 8"," 7"," 6"," 5"," 4"," 3"," 2"," 1"] ) ++ "==ABCDEFGH==" ++ "\n"


--input will look like "a2c6"
stringtomove :: String -> Move
stringtomove s | length s /= 4 = error ("error in stm: " ++ s)
stringtomove s = (ctr (s!!1) * 8 + ctc (s!!0), ctr (s!!3) * 8 + ctc (s!!2))


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

whitecastle :: Board -> Move -> Bool
whitecastle b (60, 62) = not (exists b 61) && not (exists b 62)
whitecastle b (60, 58) = not (exists b 57) && not (exists b 58) && not (exists b 59)
whitecastle b _        = False

blackcastle :: Board -> Move -> Bool
blackcastle b (4, 6) = not (exists b 5) && not (exists b 6)
blackcastle b (4, 2) = not (exists b 1) && not (exists b 2) && not (exists b 3)
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

gameover :: Board -> Bool -> Bool
gameover b t = (checkmate b t) || (stalemate b t)



-- ?? TODO?
existskings :: Board -> Bool
existskings b = (findpiece b (Just (Piece King False)) /= -1) && (findpiece b (Just (Piece King True)) /= -1)

incheck :: Board -> Bool -> Bool
incheck b True  = attacked b (findpiece b (getspot 'K')) True
incheck b False = attacked b (findpiece b (getspot 'k')) False

notcheckmove :: Board -> Move -> Bool -> Bool
notcheckmove b (o,d) t | d < 0 || d > 63 = error ("error in notcheckmove" ++ boardtostring b)
notcheckmove b (o,d) t = not (incheck (executemove b (o,d)) t) && b!!d /= (getspot 'K') && b!!d /= (getspot 'k') && existskings b








staticeval :: Board -> Int -> Double
staticeval b d | checkmate b False = minval + fromIntegral d
staticeval b d | checkmate b True  = maxval - fromIntegral d
staticeval b d | stalemate b True || stalemate b False  = 0.0
staticeval b d = staticeval' b 0

staticeval' :: Board -> Int -> Double
staticeval' b 64 = 0
staticeval' b n  = if (exists b n) then (value $ b!!n)  + (weightedposition (b!!n) n) + (staticeval' b (n+1)) else (staticeval' b (n+1))

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
weightedposition _ _                             = 0.0

value :: Spot -> Double
value (Just (Piece Pawn True))       = 10.2
value (Just (Piece Knight True))     = 30.5
value (Just (Piece Bishop True))     = 33.3
value (Just (Piece Rook True))       = 56.3
value (Just (Piece Queen True))      = 95.1
value (Just (Piece King True))       = 9999.9
value (Just (Piece Pawn False))      = -10.2
value (Just (Piece Knight False))    = -30.5
value (Just (Piece Bishop False))    = -33.3
value (Just (Piece Rook False))      = -56.3
value (Just (Piece Queen False))     = -95.1
value (Just (Piece King False))      = -9999.9
value _                              = 0



getbotmove :: Board -> Bool -> Move
getbotmove b t = findbestmove b t


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



minimax :: Board -> [Move] -> Side -> Int -> Double
minimax b m True depth |  score < -999                   = score - fromIntegral depth
            where score = staticeval b depth
minimax b m False depth | score > 999                    = score + fromIntegral depth
            where score = staticeval b depth
minimax b m ismaxing 0                                   = staticeval b 0
minimax b [] _ depth                                     = staticeval b depth
minimax b m t  depth | length m == 1                     = minimax (executemove b (head m)) (getmoves' b t) (not t) (depth-1)
minimax b (x:xs) True  depth                             = max (minimax b xs True depth) nextnode
            where nextnode = minimax (executemove b x) (getmoves' b False) False (depth-1)
minimax b (x:xs) False depth                             = min (minimax b xs False depth) nextnode
            where nextnode = minimax (executemove b x) (getmoves' b True)  True (depth-1)



--finds if a checkmate exists. (no forced checkmates)
findmate :: Board -> [Move] -> Side -> Maybe Move
findmate b [] t                    = Nothing
findmate b m True  | length m == 1 = if (findmateboo b (getmoves b True) True  True  maxmatedepth) then Just (head m) else Nothing
findmate b m False | length m == 1 = if (findmateboo b (getmoves b False) False False maxmatedepth) then Just (head m) else Nothing
findmate b (x:xs) True             = if findmateboo b (getmoves b True)  True  True  maxmatedepth then Just x else findmate b xs False
findmate b (x:xs) False            = if findmateboo b (getmoves b False) False False maxmatedepth then Just x else findmate b xs False

findmateboo :: Board -> [Move] -> Side -> Bool -> Int -> Bool
findmateboo b m t alt 0                     = checkmate b (not t)
findmateboo b m True  alt d | length m == 1 = if checkmate b False then True else findmateboo (executemove b (head m)) (getmoves b alt) True  (not alt) (d-1)
findmateboo b m False alt d | length m == 1 = if checkmate b True  then True else findmateboo (executemove b (head m)) (getmoves b alt) False (not alt) (d-1)
findmateboo b (m:ms) True  alt d            = if checkmate b False then True else findmateboo b ms True  True  d || findmateboo (executemove b m) (getmoves b alt) True  (not alt) (d-1)
findmateboo b (m:ms) False alt d            = if checkmate b True  then True else findmateboo b ms False False d || findmateboo (executemove b m) (getmoves b alt) False (not alt) (d-1)
