module GameTree where
import Types
import Chess

data Tree = Node Move Board [Tree] deriving (Show, Eq)

gettree :: State -> [Move] -> Tree
gettree s x       = Node x () []
gettree s (x:xs)  = Node x (domove )
