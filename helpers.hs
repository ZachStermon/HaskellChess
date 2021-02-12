module Helpers
( format, split, ctr, ctc,
  row, col
) where

--helper function for printing and formatting the board.
format :: String -> [String]
format s = split 8 s


--helper functions for printing and formatting the board.
split :: Int -> [a] -> [[a]]
split n = takeWhile (not.null) . map (take n) . iterate (drop n)


--takes a character and returns the row, so that an input of e2e4 turns into 32,34 for example.
ctr :: Char -> Int
ctr x = abs ((read [x]) - 8)

--same as above, helper for turning character into column.
ctc :: Char -> Int
ctc 'a' = 0
ctc 'b' = 1
ctc 'c' = 2
ctc 'd' = 3
ctc 'e' = 4
ctc 'f' = 5
ctc 'g' = 6
ctc 'h' = 7
ctc 'A' = 0
ctc 'B' = 1
ctc 'C' = 2
ctc 'D' = 3
ctc 'E' = 4
ctc 'F' = 5
ctc 'G' = 6
ctc 'H' = 7

--more helpers for grabbing the row and column.
row :: Int -> Int
row i = quot i 8
--more helpers for grabbing the row and column.
col :: Int -> Int
col i = mod i 8
