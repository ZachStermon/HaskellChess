module AI
( staticeval
, findbestmove
) where

--Imports
import Chess
import Types
import Weights
import Movechecker
import Data.List

--FOR TESTING ONLY
import Boards
import Printing
m2 = makestate mateintwo True
m2v2 = makestate mateintwov2 False
m3 = makestate mateinthree True

--declarations
maxdepth = 4
minval = -12345
maxval = 12345




--staticeval is used for evaluating the static board snapshot, diiferent pieces have different values and added weights make certain pieces worth more in different areas of the board.
-- stalemate cannot happen in testing, kings can be captured
staticeval :: State -> Double
staticeval (State{board=b})  = staticeval' b 0

staticeval' :: Board -> Int -> Double
staticeval' []     n = 0
staticeval' (x:xs) n = if null x then (staticeval' xs (n+1)) else (value x) + (weightedposition x n) + (staticeval' xs (n+1))


--this is a helper for the board weights(which can be seen in weights.hs)
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

--these are the core values for the pieces on the board.
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

--looks through the list of movesranked and returns the move with the highest value, hopefully a found checkmate.
findbestmove :: State -> Move
findbestmove s = snd . head $ minimax s (getmoves s 0) maxdepth



--helper function forsotring the list of moves and values.
sortmoves :: Bool -> [(Double, Move)] -> [(Double, Move)]
sortmoves True  = sortBy (\x y -> (fst y) `compare` (fst x))
sortmoves False = sortBy (\x y -> (fst x) `compare` (fst y))






--this is essentially a macro to help testing
test :: Board -> Turn -> Int -> [(Double, Move)]
test b t d = minimax state (filter (validmove state) $ getmoves state 0) d
  where state = (makestate b t)

--same as above, like a macro for testing.
printboard :: State -> IO()
printboard s = putStr(boardtostring (board s))






--minimax is the heart of the "AI" it looks at every possible board state up to a specified depth and will attempt to find a checkmate when possible
minimax :: State -> [Move] -> Int -> [(Double, Move)]
minimax s ms 0     = sortmoves (turn s) $ map (\m -> (staticeval (domove s m), m)) ms
minimax s ms depth = sortmoves (turn s) (map checkmoves ms)
    where checkmoves m =
            let sign = if turn s then id else (0 -)
                newstate = domove s m
                newmoves = filter (validmove newstate) $ getmoves newstate 0
            in if null newmoves
                then if incheck newstate then (sign maxval, m) else (0.0,m)
                else (fst . head $ minimax newstate newmoves (depth-1),m)











-- comment
