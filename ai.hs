module AI
( staticeval
, findbestmove
) where

import Chess
import Types
import Weights
import Movechecker

maxdepth = 1
maxmatedepth = 3
maxval = -9876
minval = 9876

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




findbestmove :: Board -> Bool -> Move
findbestmove b True | null $ getmoves b True = error("no moves in findbestmove")
findbestmove b True = snd $ maximum y
    where
        moves = getmoves b True
        y = map (\x -> (minimax (premove b x) (getmoves b True) True maxdepth, x)) moves
findbestmove b False | null $ getmoves b False = error("no moves in findbestmove")
findbestmove b False = snd $ minimum y
    where
        moves = getmoves b False
        y = map (\x -> (minimax (premove b x) (getmoves b False) False maxdepth, x)) moves



minimax :: Board -> [Move] -> Side -> Int -> Double
minimax b m True depth |  score < -999                   = score - fromIntegral depth
            where score = staticeval b depth
minimax b m False depth | score > 999                    = score + fromIntegral depth
            where score = staticeval b depth
minimax b m ismaxing 0                                   = staticeval b 0
minimax b [] _ depth                                     = staticeval b depth
minimax b m t  depth | length m == 1                     = minimax (premove b (head m)) (getmoves' b t) (not t) (depth-1)
minimax b (x:xs) True  depth                             = max (minimax b xs True depth) nextnode
            where nextnode = minimax (premove b x) (getmoves' b False) False (depth-1)
minimax b (x:xs) False depth                             = min (minimax b xs False depth) nextnode
            where nextnode = minimax (premove b x) (getmoves' b True)  True (depth-1)


--
-- --finds if a checkmate exists. (no forced checkmates)
-- findmate :: Board -> [Move] -> Side -> Maybe Move
-- findmate b [] t                    = Nothing
-- findmate b m True  | length m == 1 = if (findmateboo b (getmoves b True) True  True  maxmatedepth) then Just (head m) else Nothing
-- findmate b m False | length m == 1 = if (findmateboo b (getmoves b False) False False maxmatedepth) then Just (head m) else Nothing
-- findmate b (x:xs) True             = if findmateboo b (getmoves b True)  True  True  maxmatedepth then Just x else findmate b xs False
-- findmate b (x:xs) False            = if findmateboo b (getmoves b False) False False maxmatedepth then Just x else findmate b xs False
--
-- findmateboo :: Board -> [Move] -> Side -> Bool -> Int -> Bool
-- findmateboo b m t alt 0                     = checkmate b (not t)
-- findmateboo b m True  alt d | length m == 1 = if checkmate b False then True else findmateboo (premove b (head m)) (getmoves b alt) True  (not alt) (d-1)
-- findmateboo b m False alt d | length m == 1 = if checkmate b True  then True else findmateboo (premove b (head m)) (getmoves b alt) False (not alt) (d-1)
-- findmateboo b (m:ms) True  alt d            = if checkmate b False then True else findmateboo b ms True  True  d || findmateboo (premove b m) (getmoves b alt) True  (not alt) (d-1)
-- findmateboo b (m:ms) False alt d            = if checkmate b True  then True else findmateboo b ms False False d || findmateboo (premove b m) (getmoves b alt) False (not alt) (d-1)
