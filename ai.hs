module AI
( staticeval
, findbestmove
) where

--Imports
import Chess
import Types
import Weights
import Movechecker

--FOR TESTING ONLY
import Boards
import Printing

maxdepth = 2
maxmatedepth = 3
minval = -12345
maxval = 12345


-- stalemate cannot happen in testing, kings can be captured
staticeval :: State -> Double
staticeval (State{board=b})  = staticeval' b 0

staticeval' :: Board -> Int -> Double
staticeval' b 64 = 0
staticeval' b n  = if (exists b n) then (value $ b!!n)  + (weightedposition (b!!n) n) + (staticeval' b (n+1)) else (staticeval' b (n+1))

weightedposition :: Spot -> Int -> Double
weightedposition Nothing                     n   = 0
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

value :: Spot -> Double
value Nothing                        = 0
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


findbestmove :: State -> Move
findbestmove s = if turn s then snd $ maximum (movesranked s) else snd $ minimum (movesranked s)

-- movesranked :: State -> [(Double, Move)]
-- movesranked s | null (getmoves s 0) = error("no moves in movesranked")
-- movesranked s = map (\x -> (minimax (domove s x) (getmoves ((domove s x)) 0) maxdepth, x)) (getmoves s 0)

movesranked :: State -> [(Double, Move)]
movesranked s | null (getmoves s 0) = error("no moves in movesranked")
movesranked s = map (\x -> (minimax (domove s x) (getmoves ((domove s x)) 0) maxdepth, x)) (getmoves s 0)

-- tempfunc :: State -> [Move] -> (Double, Move) -> Maybe (Double, Move)
-- tempfunc s [] a    = a
-- tempfunc s (x:xs) a | turn s = if  move > 999 then  Just (move, x) else if move > a then tempfunc s xs move else tempfunc s xs a
--     where move = minimax (domove s x) (getmoves ((domove s x)) 0) maxdepth
-- tempfunc s (x:xs) a         = if  move < -999 then Just (move, x) else if move < a then tempfunc s xs move else tempfunc s xs a
--     where move = minimax (domove s x) (getmoves ((domove s x)) 0) maxdepth



--this function accepts a state, a list of legal moves for current player, and the depth
minimax :: State -> [Move] -> Int -> Double
minimax s m depth | attacked s enemyking                 = if turn s then maxval + fromIntegral depth else minval - fromIntegral depth
            where enemyking = findpiece (board s) (Just (Piece King (not(turn s))))
minimax s m depth | notabletomove s (getmoves s 0)       = if incheck s then
                                                           if turn s then minval - fromIntegral depth
                                                           else           maxval + fromIntegral depth
                                                           else 0
minimax s m 0                                            = staticeval s
-- minimax s m depth | score > 999 || score < -999          = score
--             where score = staticeval s
minimax s m depth | length m == 1                        = minimax state (getmoves state 0) (depth-1)
            where state = domove s (head m)
minimax s (x:xs) depth                                   = if turn s then max (minimax s xs depth) nextnode else min (minimax s xs depth) nextnode
            where nextnode = minimax state (getmoves state 0) (depth-1)
                  state = domove s x
