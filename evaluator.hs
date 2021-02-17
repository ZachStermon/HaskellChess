module Evaluator (
  staticeval
) where
import Types
import Weights
import Data.Sequence
import Data.Maybe

--staticeval is used for evaluating the static board snapshot, diiferent pieces have different values and added weights make certain pieces worth more in different areas of the board.
-- stalemate cannot happen in testing, kings can be captured
staticeval :: State -> Int
staticeval (State{board=b})  = sum $ mapWithIndex (\n x -> if isNothing x then 0 else (value x) + (weightedposition x n)) b


--this is a helper for the board weights(which can be seen in weights.hs)
weightedposition :: Spot -> Int -> Int
weightedposition (Just (Piece Pawn True))    n   =       weightedpawnwhite   !! n
weightedposition (Just (Piece Knight True))  n   =       weightedknightwhite !! n
weightedposition (Just (Piece Bishop True))  n   =       weightedbishopwhite !! n
weightedposition (Just (Piece Rook True))    n   =       weightedrookwhite   !! n
weightedposition (Just (Piece Queen True))   n   =       weightedqueenwhite  !! n
weightedposition (Just (Piece King True))    n   =       weightedkingwhite   !! n
weightedposition (Just (Piece Pawn False))   n   = -1 *  weightedpawnblack   !! n
weightedposition (Just (Piece Knight False)) n   = -1 *  weightedknightblack !! n
weightedposition (Just (Piece Bishop False)) n   = -1 *  weightedbishopblack !! n
weightedposition (Just (Piece Rook False))   n   = -1 *  weightedrookblack   !! n
weightedposition (Just (Piece Queen False))  n   = -1 *  weightedqueenblack  !! n
weightedposition (Just (Piece King False))   n   = -1 *  weightedkingblack   !! n

--these are the core values for the pieces on the board.
value :: Spot -> Int
value (Just (Piece Pawn True))       = 102
value (Just (Piece Knight True))     = 305
value (Just (Piece Bishop True))     = 333
value (Just (Piece Rook True))       = 563
value (Just (Piece Queen True))      = 951
value (Just (Piece Pawn False))      = -102
value (Just (Piece Knight False))    = -305
value (Just (Piece Bishop False))    = -333
value (Just (Piece Rook False))      = -563
value (Just (Piece Queen False))     = -951
value _                              = 0
