module Evaluator (
  staticeval,
  countpieces
) where
import Types
import Weights
import Data.Bits
import Data.Word (Word64)
--for testing only
import Boards
import Movechecker

--staticeval is used for evaluating the static board snapshot, diiferent pieces have different values and added weights make certain pieces worth more in different areas of the board.
-- stalemate cannot happen in testing, kings can be captured
-- staticeval :: State -> Int
-- staticeval (State{board=b}) = sum $ map (\n -> if empty b n then 0 else (value (getspot b n))) [0..63] -- + (weightedposition x n)

countpieces :: BitBoard -> Int
countpieces b =  (popCount (whitepawns b))   -  (popCount (blackpawns b)) +
                 (popCount (whiteknights b)) -  (popCount (blackknights b)) +
                 (popCount (whitebishops b)) -  (popCount (blackbishops b)) +
                 (popCount (whiterooks b))   -  (popCount (blackrooks b)) +
                 (popCount (whitequeens b))  -  (popCount (blackqueens b))

staticeval :: BitBoard -> Int
staticeval b  = value WhitePawn   (popCount (whitepawns b))   + value BlackPawn   (popCount (blackpawns b)) +
                value WhiteKnight (popCount (whiteknights b)) + value BlackKnight (popCount (blackknights b)) +
                value WhiteBishop (popCount (whitebishops b)) + value BlackBishop (popCount (blackbishops b)) +
                value WhiteRook   (popCount (whiterooks b))   + value BlackRook   (popCount (blackrooks b)) +
                value WhiteQueen  (popCount (whitequeens b))  + value BlackQueen  (popCount (blackqueens b))





-- + (weightedposition x n)


--this is a helper for the board weights(which can be seen in weights.hs)
weightedposition :: Piece -> Int -> Int
weightedposition WhitePawn    n   =       weightedpawnwhite   !! n
weightedposition WhiteKnight  n   =       weightedknightwhite !! n
weightedposition WhiteBishop  n   =       weightedbishopwhite !! n
weightedposition WhiteRook    n   =       weightedrookwhite   !! n
weightedposition WhiteQueen   n   =       weightedqueenwhite  !! n
weightedposition WhiteKing    n   =       weightedkingwhite   !! n
weightedposition BlackPawn    n   = -1 *  weightedpawnblack   !! n
weightedposition BlackKnight  n   = -1 *  weightedknightblack !! n
weightedposition BlackBishop  n   = -1 *  weightedbishopblack !! n
weightedposition BlackRook    n   = -1 *  weightedrookblack   !! n
weightedposition BlackQueen   n   = -1 *  weightedqueenblack  !! n
weightedposition BlackKing    n   = -1 *  weightedkingblack   !! n

--these are the core values for the pieces on the board.
-- given a piecetype and number of occurances
value :: Piece -> Int -> Int
value WhitePawn    n  = 102 * n
value WhiteKnight  n  = 305 * n
value WhiteBishop  n  = 333 * n
value WhiteRook    n  = 563 * n
value WhiteQueen   n  = 951 * n
value BlackPawn    n  = -102 * n
value BlackKnight  n  = -305 * n
value BlackBishop  n  = -333 * n
value BlackRook    n  = -563 * n
value BlackQueen   n  = -951 * n
value _            n  = 0
