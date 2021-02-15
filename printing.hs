module Printing
( getboard
, boardtostring
, stringtomove
) where

import Data.Foldable (toList)
import Data.Sequence (fromList)

import Types
import Helpers

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
getboard string = fromList $ map getspot string

showboard :: Board -> String
showboard b = map showspot (toList b)


boardtostring :: Board -> String
boardtostring b = "\n" ++ "==ABCDEFGH==" ++ "\n" ++ (unlines $ zipWith (++)(zipWith (++) ["8 ","7 ","6 ","5 ","4 ","3 ","2 ","1 "] (format $ (showboard b))) [" 8"," 7"," 6"," 5"," 4"," 3"," 2"," 1"] ) ++ "==ABCDEFGH==" ++ "\n"


--input will look like "a2c6"
stringtomove :: String -> Move
stringtomove s | length s /= 4 = error ("error in stm: " ++ s)
stringtomove s = (ctr (s!!1) * 8 + ctc (s!!0), ctr (s!!3) * 8 + ctc (s!!2))
