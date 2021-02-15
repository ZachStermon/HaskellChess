module AI
( staticeval
, findbestmove
) where

--Imports
import Chess
import Types
import Movechecker
import Data.List
import Data.Sequence hiding (null, sortBy, filter)
import Evaluator

--FOR TESTING ONLY
import Boards
import Printing
m2 = makestate mateintwo True
m2v2 = makestate mateintwov2 False
m3 = makestate mateinthree True
initi = makestate initial True

--declarations
maxdepth = 4
minval = -12345
maxval = 12345




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
                newmoves = filter (notcheckmove newstate) $ getmoves newstate 0
            in if null newmoves
                then if incheck newstate then (sign maxval, m) else (0.0,m)
                else (fst . head $ minimax newstate newmoves (depth-1),m)







-- lookup table that remebers a specific boards evaluation
type Table = [([Move], Double)]

lookuptable :: Table -> [Move] -> Bool
lookuptable t m = m `elem` (map fst t)

geteval :: Table -> [Move] -> Int -> Double
geteval t m n = if fst (t!!n) == m then snd (t!!n) else geteval t m (n+1)

addtotable :: Table -> State -> Table
addtotable t s = ((history s), (staticeval s)):t




-- infinite rose tree of all possible games
listtrees :: State -> [Move] -> [GameTree]
listtrees s []      = [makenode s []]
listtrees s (x:xs)  = [makenode state (listtrees state (getmoves state 0))] ++ listtrees s xs
  where state = domove s x

maketree :: State -> GameTree
maketree s = makenode s (listtrees s (getmoves s 0))

makenode :: State -> [GameTree] -> GameTree
makenode s n = GameTree{state = s, eval = (staticeval s), children = n}

data GameTree = GameTree { state :: State,
                           eval :: Double,
                           children :: [GameTree]
                          } deriving (Show, Eq)

getnode :: GameTree -> GameTree
getnode g = g{children = []}

getchildnodes :: GameTree -> [GameTree]
getchildnodes g = map (\x -> getnode x) (children g)


instance Show State where
  show s = boardtostring (board s) ++ show (turn s) ++ "\n" ++ show (history s) ++ "\n"

-- comment
