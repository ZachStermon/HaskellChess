module Helpers
( format, split, ctr, ctc,
  row, col
) where

format :: String -> [String]
format s = split 8 s

split :: Int -> [a] -> [[a]]
split n = takeWhile (not.null) . map (take n) . iterate (drop n)

ctr :: Char -> Int
ctr x = abs ((read [x]) - 8)

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


row :: Int -> Int
row i = quot i 8

col :: Int -> Int
col i = mod i 8
