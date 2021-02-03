module Boards where

import Printing

-- Board States
staleboard     = getboard ("kr--------r-----------------------------r-------P-------K-------")
checkmateboard = getboard ("----k------------------------------q---------------PPb-----QKB--")
testboard      = getboard ("rnbqkb-rppp---pp-----------nNp-Q--B-------------PPPP-PPPRNB-K--R")
mateinthree    = getboard ("---------------kp------p----B-p----P-pQ--------PP-----PK-----q--")
mateinthreev2  = getboard ("---rk--rp----pppb-p-------b-----Nq--PPn--P------P-P-N-PPR-B-QK-R") --best moves are as follows: (b4e1 (33,60)),(e1f1 (60,61)),(d8d1 (3,59))
mateintwo      = getboard ("---------Q--------------k-----------------K---------------------")
mateintwov2    = getboard ("------k------ppp----b---p-q----------K------P---r-----PP---Q---R") --best moves are as follows: (c5f5 (26,29)),(f5f2 (29,53))
initial        = getboard ("rnbqkbnr" ++ "pppppppp" ++ empty ++ empty ++ empty ++ empty ++ "PPPPPPPP" ++ "RNBQKBNR")
empty          = "--------"
