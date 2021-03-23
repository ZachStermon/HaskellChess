module Openings where

import System.IO
import Printing
import Types
import Data.Char


type FilePath = String
data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode
-- data OpeningTree = OpeningTree { moveorder  :: [Move],
--                                  eco :: String,
--                                  opening :: String,
--                                  variation :: String,
--                                  children  :: [OpeningTree]
--                                } deriving (Show, Eq)
data Tree = Tree {move :: Move, children :: [Tree]} deriving (Show, Eq)

-- let tree = [(48,32),(48,40),(49,33),(49,41),(50,34),(50,42),(51,35),(51,43),(52,36),(52,44),(53,37),(53,45),(54,38),(54,46),(55,39),(55,47),(57,42),(57,40),(62,47),(62,45)]

createtree :: [[Move]] -> Int -> [Tree]
createtree ms 22 = []
createtree ms n = map (\x -> Tree {move = x, children = createtree ms (n+1)}) (removeDuplicates (map (\p -> if length p < n then p!!n else (0,0)) ms))

removeDuplicates :: [Move] -> [Move]
removeDuplicates xs = removeDuplicatesHelper xs []
   where
     removeDuplicatesHelper [] _ = []
     removeDuplicatesHelper (x:xs) found
       | elem x found = removeDuplicatesHelper xs found
       | otherwise    = x:removeDuplicatesHelper xs (x:found)


-- main = do
--     contents <- readFile "out2.pgn"
--     let tree = OpeningTree{moveorder = [], eco = [], opening = [], variation = [], children = []}
--         content = lines contents
--     show $ setuptree tree content


-- main = do
--     contents <- readFile "out2.pgn"
--     let tree = OpeningTree{moveorder = [], eco = [], opening = [], variation = [], children = []}
--         content = lines contents
--     show $ setuptree tree content

-- setuptree :: OpeningTree -> [String] -> OpeningTree
-- setuptree tree (s1,s2,[],m1,[],ss) | s1!!0 /= '[' || s2!!0 /= '[' = error("non bracket non variation")
--                                       | otherwise                    = tree {moveorder = getmoveorder m1, eco = s1, opening = s2}
-- setuptree tree (s1,s2,s3,[],m1,[],ss) | s1!!0 /= '[' || s2!!0 /= '[' || s3!!0 /= '[' = error("non bracket variation m1")
--                                       | otherwise                    = tree {moveorder = getmoveorder m1, eco = s1, opening = s2, variation = s3}
-- setuptree tree (s1,s2,s3,[],m1,m2,[],ss) | s1!!0 /= '[' || s2!!0 /= '[' || s3!!0 /= '[' = error("non bracket variation m2")
--                                       | otherwise                    = tree {moveorder = getmoveorder (m1++m2), eco = s1, opening = s2, variation = s3}
-- setuptree tree (s1,s2,s3,[],m1,m2,m3,[],ss) | s1!!0 /= '[' || s2!!0 /= '[' || s3!!0 /= '[' = error("non bracket variation m3")
--                                       | otherwise                    = tree {moveorder = getmoveorder (m1++m2++m3), eco = s1, opening = s2, variation = s3}

main = do
    contents <- readFile "out4.pgn"
    let list = map getmovesfromline (lines contents)
    putStrLn $ show (createtree list 0)

-- main = do
--     contents <- readFile "out1.pgn"
--     writeFile "out3.pgn" (unlines ( (filter (\s -> s /= [] && s!!0 /= '[') (map translateline (lines contents)))))

-- translatemoves :: String -> [String]
-- translatemoves (s:ss) = (translateline s):(translatemoves ss)

-- getmoveorder :: String -> [Move]
-- getmoveorder s = unwords $ map stringtomove (words s)

getmovesfromline :: String -> [Move]
getmovesfromline s = map tomove (words s)

tomove :: String -> Move
tomove ('(':m1:m2:',':m3:m4:')':xs) = (toEnum $ digitToInt m1 * 10 + digitToInt m2,toEnum $ digitToInt m3 * 10 + digitToInt m4)
tomove ('(':m1:',':m2:')':xs)       = (toEnum $ digitToInt m1,toEnum $ digitToInt m2)
tomove ('(':m1:m2:',':m3:')':xs) = (toEnum $ digitToInt m1 * 10 + digitToInt m2,toEnum $ digitToInt m3)
tomove ('(':m1:',':m3:m4:')':xs) = (toEnum $ digitToInt m1,toEnum $ digitToInt m3 * 10 + digitToInt m4)

func :: [String] -> [String]
func [] = []
func s | length s == 1 = s
func (x1:x2:xs) = if x2!!0 /= '1' || x2!!1 /= '.' then (x1++x2):(func xs) else (x1:(func (x2:xs)))

translateline :: String -> String
translateline [] = []
translateline s | s!!0 == '[' = s
                | otherwise   = unwords $ map translatewords (words s)

translatewords :: String -> String
translatewords s  | length s == 4 = show $ stringtomove s
                  | otherwise     = s

stringisamove :: String -> Bool
stringisamove [] = False
stringisamove s | length s /= 4 = False
                | otherwise     = True
