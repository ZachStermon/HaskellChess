module GameTree where
import Types
import Boards
import Chess



translate :: String -> Move
translate ""  = error("Empty")
translate " " = error("Empty")
translate " "

s = makestate initial True
