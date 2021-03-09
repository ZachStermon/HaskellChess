module AI
( staticeval
, findbestmove
) where

--Imports
import Chess
import Types
import Movechecker
import Data.List
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
rookmate1 = makestate rookmate True

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
findbestmove s = (if turn s then maximum else minimum) $ foreach s







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


done :: State -> Int
done s | turn s     = if incheck s then minval else 0
       | otherwise  = if incheck s then maxval else 0

--does not evaluate checkmate or stalemate at leaves
--when depth is zero, return max of static eval and alpha and min of that with beta
--alpha = best eval for current player so far
--beta  = best eval for oposing player so far
--Minimax with alpha-beta pruning
minimaxalphabeta :: Int -> Int -> Int -> State -> Int
minimaxalphabeta 0     a b s = a `max` ((if turn s then id else (0-)) (staticeval s)) `min` b
minimaxalphabeta depth a b s | null moves = (if turn s then id else (0-)) (done s )           --checkmate or stalemate
                             | otherwise  = recurse a b moves --otherwise call recursive function
    where moves = getmoves s
          recurse a b []  = a
          recurse a b (m:ms) | alpha >= b = alpha
                             | otherwise = recurse alpha b ms
                             where alpha = a `max` (-minimaxalphabeta (depth-1) (-b) (-a) (domove s m))

--sortBy (\x y -> compare (staticeval (domove s x)) (staticeval (domove s y))) $

recurses:: State -> Int -> Int -> [Move] -> [Move] -> (Int, [Move])
recurses s a b h []     = (a,h)
recurses s a b h (m:ms) =
                        let (x,y) = negamax (domove s m) (h) 0 (-b) (-a)
                            (a',h') = if(a >= -x) then (a,h) else (-x,m:y)
                        in  if(a' >= b) then (a',h') else recurses s (a') (b) (h') ms


--alpha is best eval for current player
--beta is best eval for opponent
negamax :: State -> [Move] -> Int -> Int -> Int -> (Int, [Move])
negamax s h 0 a b = (a `max` ((if turn s then id else (0-)) (staticeval s)) `min` b,h)
negamax s h d a b | whitekings (board s) == 0 = (if turn s then minval else maxval, h)
                  | blackkings (board s) == 0 = (if turn s then maxval else minval, h)
                  | otherwise = recurse (a,h) (b,h) moves
                  where moves = getsudomoves s
                        recurse (a,as) (b,bs) [] = (a,as)
                        recurse (a,as) (b,bs) (m:ms) | newalpha >= b = (newalpha, alpha')
                                                     | otherwise = recurse (newalpha,alpha') (b,bs) ms
                                                     where (alpha,alphal) = negamax (domove s m) (h) (d-1) (-b) (-a)
                                                           (newalpha,alpha') = if a >= (-alpha)
                                                                               then (a,as)
                                                                               else (-alpha,m:alphal)

moves = getsudomoves m3v3
fun m = negamax (domove m3v3 m) [m] 0 (-9999) (9999)











--No need to recompute move

foreach :: State -> [(Int, Move)]
foreach s = Prelude.zip (map ((0-) . minimaxalphabeta maxdepth (-9999) (9999)) (map (domove s) moves)) moves
  where moves = getmoves s

nstat :: State -> Int
nstat s | turn s    = staticeval s
        | otherwise = -staticeval s



--helper function for sorting the list of moves and values.
-- sortmoves :: State -> [(Int, Move)] -> [(Int, Move)]
-- sortmoves s ms = Data.List.sortBy


--           eval move  a     b
type Mini = (Int, Move, Int , Int)

-- evalsomemoves :: State -> [Move] -> Int -> Int -> Int -> Mini
-- evalsomemoves s (m:ms) alpha beta 0 =

hide' (a,b,c,d) = (a,b)






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



fst'  (a,b,c,d) = a
snd'  (a,b,c,d) = b
trd'  (a,b,c,d) = c
fth'  (a,b,c,d) = d

maximum' [(a,b,c)] = (maximum a,b,c)
minimum' [(a,b,c)] = (minimum a,b,c)



-- -- lookup table that remebers a specific boards evaluation
-- type Table = [(Board, Int)]
--
-- lookuptable :: Table -> Board -> Bool
-- lookuptable t m = m `elem` (map fst t)
--
-- geteval :: Table -> Board -> Int -> Int
-- geteval t m n = if fst (t!!n) == m then snd (t!!n) else geteval t m (n+1)
--
-- addtotable :: Table -> State -> Table
-- addtotable t s = ((board s), (staticeval s)):t
--
--
--
--
-- -- infinite rose tree of all possible games
-- listtrees :: State -> [Move] -> [GameTree]
-- listtrees s []      = [makenode s []]
-- listtrees s (x:xs)  = [makenode state (listtrees state (getmoves state))] ++ listtrees s xs
--   where state = domove s x
--
-- maketree :: State -> GameTree
-- maketree s = makenode s (listtrees s (getmoves s))
--
-- makenode :: State -> [GameTree] -> GameTree
-- makenode s n = GameTree{state = s, eval = (staticeval s), children = n}
--
-- data GameTree = GameTree { state :: State,
--                            eval :: Int,
--                            children :: [GameTree]
--                           } deriving (Show, Eq)
--
-- getnode :: GameTree -> GameTree
-- getnode g = g{children = []}
--
-- getchildnodes :: GameTree -> [GameTree]
-- getchildnodes g = map (\x -> getnode x) (children g)


































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










-- minimaxwithpruning :: State -> [Move] -> Int -> Int -> Int -> (Int, Move)
-- minimaxwithpruning s [] a b 0     = if incheck s then worstval s else (0,(0,0))
-- minimaxwithpruning s ms a b 0     =  ((if turn s then maximum else minimum) $ map (evalonemove s) ms)
-- minimaxwithpruning s ms a b depth | turn s =
--   let stepthrough [] best = best
--       stepthrough (m:mz) best =
--         let newstate = domove s m
--             (c1,c2)  = minimaxwithpruning newstate (getmoves newstate) (fst best) b (depth-1)
--             newbest  = max' best (c1,m)
--         in if b <= (fst newbest) then best else stepthrough mz newbest
--   in  stepthrough ms (worstval s)
--                                 | otherwise =
--   let stepthrough [] best = best
--       stepthrough (m:mz) best =
--         let newstate = domove s m
--             (c1,c2)  = minimaxwithpruning newstate (getmoves newstate) a (fst best) (depth-1)
--             newbest  = min' best (c1,m)
--         in if (fst newbest) <= a then best else stepthrough mz newbest
--   in  stepthrough ms (worstval s)








-- comment
