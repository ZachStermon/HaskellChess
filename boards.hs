module Boards where

import Printing

-- Board States
staleboard     = getboard ("kr--------r-----------------------------r-------P-------K-------")
checkmateboard = getboard ("----k------------------------------q---------------PPb-----QKB--")
testboard      = getboard ("rnbqkb-rppp---pp-----------nNp-Q--B-------------PPPP-PPPRNB-K--R")
mateinthree    = getboard ("---------------kp------p----B-p----P-pQ--------PP-----PK-----q--")
mateinthreev2  = getboard ("---rk--rp----pppb-p-------b-----Nq--PPn--P------P-P-N-PPR-B-QK-R") --best moves are as follows: (b4e1 (33,60)),(e1f1 (60,61)),(d8d1 (3,59))
mateinthreev3  = getboard ("-------K-----kB-------------q--------------Q--------------------") --best moves are as follows: (d3d5),(f7f6),(d5e5),(f6f5)
mateintwo      = getboard ("---------Q--------------k-----------------K---------------------")
mateintwov2    = getboard ("Q--------------------k------R--p-------P--B-----------K---------") --best moves are as follows: (a8e8 (0,4)),(f6g7(21,14)),(e5h5(28,31)) WHITES TURN
mateintwov3    = getboard ("------k------ppp----b---p-q----------K------P---r-----PP---Q---R") --best moves are as follows: (c5f5 (S26,29)),(f5f2 (29,53))
mateinthreev4  = getboard ("-------------B-----------K--------------------------R---k-------")
mateinfour     = getboard ("-------------B-----------K-----------------R------------k-------")
initial        = getboard ("rnbqkbnrpppppppp--------------------------------PPPPPPPPRNBQKBNR")

rookmate = getboard $ "------k-R-------------------K-----------------------------------"
