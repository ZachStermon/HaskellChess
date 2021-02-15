module Evaluator (
  staticeval
) where
import Types
import Weights
import Data.Sequence
import Data.Maybe

--staticeval is used for evaluating the static board snapshot, diiferent pieces have different values and added weights make certain pieces worth more in different areas of the board.
-- stalemate cannot happen in testing, kings can be captured
staticeval :: State -> Double
staticeval (State{board=b})  = staticeval' b 0

staticeval' :: Board -> Int -> Double
staticeval' b 64 = 0
staticeval' b n  = if isNothing spot then (staticeval' b (n+1)) else (value spot) + (weightedposition spot n) + (staticeval' b (n+1))
  where spot = index b n



--this is a helper for the board weights(which can be seen in weights.hs)
weightedposition :: Spot -> Int -> Double
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
