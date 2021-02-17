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
m3v2 = makestate mateinthreev2 True
m3v3 = makestate mateinthreev3 True
initi = makestate initial True

--declarations
maxdepth = 4
minval = -12345
maxval = 12345


--Auxillary test function
moveit :: (State,(Int,Move)) -> (State, (Int, Move))
moveit (s,(_,m)) = (newstate, findbestmove newstate)
  where newstate = domove s m







--looks through the list of movesranked and returns the move with the highest value, hopefully a found checkmate.
findbestmove :: State -> (Int, Move)
findbestmove s = minimaxwithpruning s (getmoves s) minval maxval maxdepth



--helper function forsotring the list of moves and values.
sortmoves :: Bool -> [(Int, Move)] -> [(Int, Move)]
sortmoves True  = sortBy (\x y -> (fst y) `compare` (fst x))
sortmoves False = sortBy (\x y -> (fst x) `compare` (fst y))



worstval :: State -> (Int,Move)
worstval s | turn s     = (minval,(0,0))
           | otherwise  = (maxval,(0,0))

--this is essentially a macro to help testing
-- test :: Board -> Turn -> Int -> (Int, Move)
-- test b t d = minimax state (filter (validmove state) $ getmoves state) d
--   where state = (makestate b t)

--like minimax but utilizes a/b pruning to eliminate bad nodes
-- minimaxwithpruning :: State -> [Move] -> Int -> Int -> Int -> Mini
-- minimaxwithpruning s [] a b 0     = error (show s)
-- minimaxwithpruning s ms a b 0     = (\(x,y) -> (x,y,a,b)) ((if turn s then maximum else minimum) $ map (evalonemove s) ms)
-- minimaxwithpruning s ms a b depth | notabletomove s ms = (\(x,y) -> (x,y,a,b)) $ if incheck s then worstval s else (0,(0,0))
-- minimaxwithpruning s ms a b depth | turn s =
--   let stepthrough [] best = best
--       stepthrough (m:mz) best =
--         let newstate = domove s m
--             (c1,c2,c3,c4) = minimaxwithpruning newstate (getmoves newstate) (fst best) b (depth-1)
--             newbest = max best (c1,m)
--         in if b <= (fst newbest) then best else stepthrough mz newbest
--   in  (\(x,y) -> (x,y,x,b)) $ stepthrough (getmoves s) (worstval s)
--                                 | otherwise =
--   let stepthrough [] best = best
--       stepthrough (m:mz) best =
--         let newstate = domove s m
--             (c1,c2,c3,c4) = minimaxwithpruning newstate (getmoves newstate) a (fst best) (depth-1)
--             newbest = min best (c1,m)
--         in if (fst newbest) <= a then best else stepthrough mz newbest
--   in  (\(x,y) -> (x,y,a,x)) $ stepthrough (getmoves s) (worstval s)

min' (a,b) (c,d) = if a <= c then (a,b) else (c,d)
max' (a,b) (c,d) = if a >= c then (a,b) else (c,d)

minimaxwithpruning :: State -> [Move] -> Int -> Int -> Int -> (Int, Move)
minimaxwithpruning s [] a b 0     = if incheck s then worstval s else (0,(0,0))
minimaxwithpruning s ms a b 0     =  ((if turn s then maximum else minimum) $ map (evalonemove s) ms)
minimaxwithpruning s ms a b depth | notabletomove s (getsudomoves s) = error ("test123")
minimaxwithpruning s ms a b depth | turn s =
  let stepthrough [] best = best
      stepthrough (m:mz) best =
        let newstate = domove s m
            (c1,c2)  = minimaxwithpruning newstate (getmoves newstate) (fst best) b (depth-1)
            newbest  = max' best (c1,m)
        in if b <= (fst newbest) then best else stepthrough mz newbest
  in  stepthrough ms (worstval s)
                                | otherwise =
  let stepthrough [] best = best
      stepthrough (m:mz) best =
        let newstate = domove s m
            (c1,c2)  = minimaxwithpruning newstate (getmoves newstate) a (fst best) (depth-1)
            newbest  = min' best (c1,m)
        in if (fst newbest) <= a then best else stepthrough mz newbest
  in  stepthrough ms (worstval s)




--           eval move  a     b
type Mini = (Int, Move, Int , Int)

-- evalsomemoves :: State -> [Move] -> Int -> Int -> Int -> Mini
-- evalsomemoves s (m:ms) alpha beta 0 =

hide' (a,b,c,d) = (a,b)


-- evalsomemoves :: State -> [Move] -> Int -> Int -> (Int,Move) -> Int -> (Int,Move)
-- evalsomemoves s (m:ms) a b best depth | Prelude.length (m:ms) == 1 = func best eval
--                                       | otherwise  = if tester then best
--                                                      else evalsomemoves s ms alpha beta (func best eval) depth
--                     where eval      = if null nextmoves
--                                       then if incheck nextstate then (if turn nextstate then minval else maxval,m) else (0,m)
--                                       else minimaxwithpruning nextstate nextmoves alpha beta (depth-1)
--                           nextstate = domove s m
--                           nextmoves = getmoves nextstate
--                           alpha     = if turn nextstate then max a (fst eval) else a
--                           beta      = if turn nextstate then b else min b (fst eval)
--                           tester    = beta <= alpha
--                           func      = if turn s then max' else min'
-- evalsomemoves s m a b best depth = error(show s)



evalonemove :: State -> Move -> (Int,Move)
evalonemove s m = (staticeval (domove s m),m)





minimax :: State -> [Move] -> Int -> (Int,Move)
minimax s ms depth | depth == 0 = (if turn s then maximum else minimum) $ map (evalonemove s) ms
                   | otherwise  = (if turn s then maximum else minimum) $ map checkmoves ms
    where checkmoves m =
            let sign = if turn s then id else (0 -)
                newstate = domove s m
                newmoves = getmoves newstate
                in if null newmoves
                    then if incheck newstate then (sign maxval,m) else (0,m)
                    else (fst $ minimax newstate newmoves (depth-1),m)



endgame :: State -> Int
endgame s   | incheck s = if turn s then maxval else minval
            | otherwise = 0

--minimax is the heart of the "AI" it looks at every possible board state up to a specified depth and will attempt to find a checkmate when possible
-- minimax :: Table -> State -> [Move] -> Int -> (Int, Move, Table)
-- minimax t s ms 0 | turn s    = maximum' (map (\m -> (staticeval (domove s m), m, t)) ms)
--                  | otherwise = minimum' (map (\m -> (staticeval (domove s m), m, t)) ms)
-- minimax t s ms depth = minormax $ (map checkmoves ms)
--     where minormax = if turn s then maximum' else minimum'
--           checkmoves m =
--             let sign = if turn s then id else (0 -)
--                 newstate = domove s m
--                 newmoves = getmoves newstate
--             in if null newmoves
--                 then if incheck newstate then (sign maxval,m,t) else (0,m,t)
--                 else (fst' $ minimax t newstate newmoves (depth-1),m,t)

fst'  (a,b,c,d) = a
snd'  (a,b,c,d) = b
trd'  (a,b,c,d) = c
fth'  (a,b,c,d) = d

maximum' [(a,b,c)] = (maximum a,b,c)
minimum' [(a,b,c)] = (minimum a,b,c)



-- lookup table that remebers a specific boards evaluation
type Table = [(Board, Int)]

lookuptable :: Table -> Board -> Bool
lookuptable t m = m `elem` (map fst t)

geteval :: Table -> Board -> Int -> Int
geteval t m n = if fst (t!!n) == m then snd (t!!n) else geteval t m (n+1)

addtotable :: Table -> State -> Table
addtotable t s = ((board s), (staticeval s)):t




-- infinite rose tree of all possible games
listtrees :: State -> [Move] -> [GameTree]
listtrees s []      = [makenode s []]
listtrees s (x:xs)  = [makenode state (listtrees state (getmoves state))] ++ listtrees s xs
  where state = domove s x

maketree :: State -> GameTree
maketree s = makenode s (listtrees s (getmoves s))

makenode :: State -> [GameTree] -> GameTree
makenode s n = GameTree{state = s, eval = (staticeval s), children = n}

data GameTree = GameTree { state :: State,
                           eval :: Int,
                           children :: [GameTree]
                          } deriving (Show, Eq)

getnode :: GameTree -> GameTree
getnode g = g{children = []}

getchildnodes :: GameTree -> [GameTree]
getchildnodes g = map (\x -> getnode x) (children g)






instance Show State where
  show s = boardtostring (board s) ++ show (turn s) ++ "\n" ++ show (history s) ++ "\n"

-- comment
