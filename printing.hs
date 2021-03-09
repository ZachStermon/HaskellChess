module Printing
( getboard
, boardtostring
, stringtomove
, boardtobit
, changevariable
, getspot
, getblackpieces
, getwhitepieces
, printword
) where


import Data.Foldable (toList)
import Data.Sequence (fromList, index, mapWithIndex)
import Data.Word (Word8, Word64)
import Data.Bits
import Text.Printf

import Types
import Helpers

showspot :: Piece -> Char
showspot WhitePawn   = 'P'
showspot WhiteKnight = 'N'
showspot WhiteBishop = 'B'
showspot WhiteRook   = 'R'
showspot WhiteQueen  = 'Q'
showspot WhiteKing   = 'K'
showspot BlackPawn   = 'p'
showspot BlackKnight = 'n'
showspot BlackBishop = 'b'
showspot BlackRook   = 'r'
showspot BlackQueen  = 'q'
showspot BlackKing   = 'k'
showspot _           = '-'

abstractspot :: Char -> Piece
abstractspot 'P' = WhitePawn
abstractspot 'N' = WhiteKnight
abstractspot 'B' = WhiteBishop
abstractspot 'R' = WhiteRook
abstractspot 'Q' = WhiteQueen
abstractspot 'K' = WhiteKing
abstractspot 'p' = BlackPawn
abstractspot 'n' = BlackKnight
abstractspot 'b' = BlackBishop
abstractspot 'r' = BlackRook
abstractspot 'q' = BlackQueen
abstractspot 'k' = BlackKing
abstractspot  _  = Void

extendfen :: String -> String
extendfen ('8':xs) = "--------" ++ extendfen xs
extendfen ('7':xs) = "-------" ++ extendfen xs
extendfen ('6':xs) = "------" ++ extendfen xs
extendfen ('5':xs) = "-----" ++ extendfen xs
extendfen ('4':xs) = "----" ++ extendfen xs
extendfen ('3':xs) = "---" ++ extendfen xs
extendfen ('2':xs) = "--" ++ extendfen xs
extendfen ('1':xs) = "-" ++ extendfen xs
extendfen ('/':xs) = extendfen xs
extendfen (x:xs)   = [x] ++ extendfen xs
extendfen s = s

getboard :: String -> Board
getboard string = fromList $ map abstractspot string

showboard :: Board -> String
showboard b = map showspot (toList b)

bbstart = BitBoard {whitepawns = 0,blackpawns = 0, whiteknights = 0,blackknights = 0,whitebishops = 0,blackbishops = 0,whiterooks = 0,blackrooks = 0,whitequeens = 0,blackqueens = 0,whitekings = 0,blackkings = 0, whitepieces = 0, blackpieces = 0, occupied = 0}
--This function will iterate through a board (Sequence of Maybe Pieces), and when encountering a piece, we set the bit to 1 at that location on the corresponding pieces bitboard.
boardtobit :: Board -> BitBoard
boardtobit b = bb{whitepieces = wp, blackpieces = bp, occupied = wp.|.bp}
  where bb = btb bbstart b 0
        wp = getwhitepieces bb
        bp = getblackpieces bb

btb :: BitBoard -> Board -> Position -> BitBoard
btb bb b 64 = bb
btb bb b n  = btb (changevariable bb (index b (fromEnum n)) n) b (n+1)


--Bool
changevariable :: BitBoard -> Piece -> Position -> BitBoard
changevariable bb Void        n = bb
changevariable bb BlackPawn   n = bb{blackpawns   = blackpawns   bb `setBit` (fromEnum n)}
changevariable bb WhitePawn   n = bb{whitepawns   = whitepawns   bb `setBit` (fromEnum n)}
changevariable bb BlackKnight n = bb{blackknights = blackknights bb `setBit` (fromEnum n)}
changevariable bb WhiteKnight n = bb{whiteknights = whiteknights bb `setBit` (fromEnum n)}
changevariable bb BlackBishop n = bb{blackbishops = blackbishops bb `setBit` (fromEnum n)}
changevariable bb WhiteBishop n = bb{whitebishops = whitebishops bb `setBit` (fromEnum n)}
changevariable bb BlackRook   n = bb{blackrooks   = blackrooks   bb `setBit` (fromEnum n)}
changevariable bb WhiteRook   n = bb{whiterooks   = whiterooks   bb `setBit` (fromEnum n)}
changevariable bb BlackQueen  n = bb{blackqueens  = blackqueens  bb `setBit` (fromEnum n)}
changevariable bb WhiteQueen  n = bb{whitequeens  = whitequeens  bb `setBit` (fromEnum n)}
changevariable bb BlackKing   n = bb{blackkings   = blackkings   bb `setBit` (fromEnum n)}
changevariable bb WhiteKing   n = bb{whitekings   = whitekings   bb `setBit` (fromEnum n)}


getspot :: BitBoard -> Position -> Piece
getspot bb p     | testBit (whitepawns bb)   n = WhitePawn
                 | testBit (blackpawns bb)   n = BlackPawn
                 | testBit (whiteknights bb) n = WhiteKnight
                 | testBit (blackknights bb) n = BlackKnight
                 | testBit (whitebishops bb) n = WhiteBishop
                 | testBit (blackbishops bb) n = BlackBishop
                 | testBit (whiterooks bb)   n = WhiteRook
                 | testBit (blackrooks bb)   n = BlackRook
                 | testBit (whitequeens bb)  n = WhiteQueen
                 | testBit (blackqueens bb)  n = BlackQueen
                 | testBit (whitekings bb)   n = WhiteKing
                 | testBit (blackkings bb)   n = BlackKing
                 | otherwise                   = Void
                    where n = fromEnum p

--concatenation of all white bitboards
getwhitepieces :: BitBoard -> Word64
getwhitepieces bb = (whitepawns bb.|.whiteknights bb.|.whitebishops bb.|.whiterooks bb.|.whitequeens bb.|.whitekings bb)

--concatenation of all black bitboards
getblackpieces :: BitBoard -> Word64
getblackpieces bb = (blackpawns bb.|.blackknights bb.|.blackbishops bb.|.blackrooks bb.|.blackqueens bb.|.blackkings bb)











bittoboard :: BitBoard -> [Piece]
bittoboard bb = map (getspot bb) [0..63]

boardtostring :: Board -> String
boardtostring b = "\n" ++ "==ABCDEFGH==" ++ "\n" ++ (unlines $ zipWith (++)(zipWith (++) ["8 ","7 ","6 ","5 ","4 ","3 ","2 ","1 "] (format $ (showboard b))) [" 8"," 7"," 6"," 5"," 4"," 3"," 2"," 1"] ) ++ "==ABCDEFGH==" ++ "\n"


--input will look like "a2c6"
stringtomove :: String -> Move
stringtomove s | length s /= 4 = error ("error in stm: " ++ s)
stringtomove s = (ctr (s!!1) * 8 + ctc (s!!0), ctr (s!!3) * 8 + ctc (s!!2))

printboard :: BitBoard -> IO()
printboard bb = putStr(boardtostring $ fromList $ bittoboard bb)


instance Show BitBoard where
  show bb = boardtostring $ fromList $ bittoboard bb

instance Show State where
  show s = show (board s) ++ show (turn s) ++ "\n" ++ show (history s) ++ "\n"

printword :: Word64 -> IO()
printword w = printf "value is:   %064b\n" w

-- instance Show BitBoard where
--   show bb = printf "whitepawns   is: %064b\n" (whitepawns bb) ++
--             printf "blackpawns   is: %064b\n" (blackpawns bb) ++
--             printf "whiteknights is: %064b\n" (whiteknights bb) ++
--             printf "blackknights is: %064b\n" (blackknights bb) ++
--             printf "whitebishops is: %064b\n" (whitebishops bb) ++
--             printf "blackbishops is: %064b\n" (blackbishops bb) ++
--             printf "whiterooks   is: %064b\n" (whiterooks bb) ++
--             printf "blackrooks   is: %064b\n" (blackrooks bb) ++
--             printf "whitequeens  is: %064b\n" (whitequeens bb) ++
--             printf "blackqueens  is: %064b\n" (blackqueens bb) ++
--             printf "whitekings   is: %064b\n" (whitekings bb) ++
--             printf "blackkings   is: %064b\n" (blackkings bb) ++
--             printf "blackpieces  is: %064b\n" (blackpieces bb) ++
--             printf "whitepieces  is: %064b\n" (whitepieces bb) ++
--             printf "occupied     is: %064b\n" (occupied bb)
