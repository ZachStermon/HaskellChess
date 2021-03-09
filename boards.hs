module Boards where

import Printing
import Types
-- Board States
staleboard     = boardtobit $ getboard ("kr--------r-----------------------------r-------P-------K-------")
checkmateboard = boardtobit $ getboard ("----k------------------------------q---------------PPb-----QKB--")
checkboard     = boardtobit $ getboard ("----k------------------------------q----------b----PP-Q-----KB--")
testboard      = boardtobit $ getboard ("rnbqkb-rppp---pp-----------nNp-Q--B-------------PPPP-PPPRNB-K--R")
mateinthree    = boardtobit $ getboard ("---------------kp------p----B-p----P-pQ--------PP-----PK-----q--")
mateinthreev2  = boardtobit $ getboard ("---rk--rp----pppb-p-------b-----Nq--PPn--P------P-P-N-PPR-B-QK-R") --best moves are as follows: (b4e1 (33,60)),(e1f1 (60,61)),(d8d1 (3,59))
mateinthreev3  = boardtobit $ getboard ("-------K-----kB-------------q--------------Q--------------------") --best moves are as follows: (d3d5),(f7f6),(d5e5),(f6f5)
mateintwo      = boardtobit $ getboard ("---------Q--------------k-----------------K---------------------")
mateintwov2    = boardtobit $ getboard ("Q--------------------k------R--p-------P--B-----------K---------") --best moves are as follows: (a8e8 (0,4)),(f6g7(21,14)),(e5h5(28,31)) WHITES TURN
mateintwov3    = boardtobit $ getboard ("------k------ppp----b---p-q----------K------P---r-----PP---Q---R") --best moves are as follows: (c5f5 (S26,29)),(f5f2 (29,53))
mateinthreev4  = boardtobit $ getboard ("-------------B-----------K--------------------------R---k-------")
mateinfour     = boardtobit $ getboard ("-------------B-----------K-----------------R------------k-------")
initial        = boardtobit $ getboard ("rnbqkbnrpppppppp--------------------------------PPPPPPPPRNBQKBNR")
rookmate       = boardtobit $ getboard $ "------k-R-------------------K-----------------------------------"


bbinit         = BitBoard {whitepawns = 71776119061217280, blackpawns = 65280, whiteknights = 4755801206503243776, blackknights = 66, whitebishops = 2594073385365405696, blackbishops = 36, whiterooks = 9295429630892703744, blackrooks = 129, whitequeens = 576460752303423488, blackqueens = 8, whitekings = 1152921504606846976, blackkings = 16, whitepieces = 18446462598732840960, blackpieces = 65535, occupied = 18446462598732906495}
