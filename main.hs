import chess

--This function creates the initial chess board position
--Then the function calls the main play function
main :: IO()
main = play initial

--This function is played repeatedly for each players move
play :: Board -> IO()
play b = do showboard b
                putStr "move(ORIGIN DESTINATION): "
                move <- getLine
                movement b stringtomove(move)


--This function tests if the move is valid and plays it if it is
movement :: Board -> Move -> IO()
movement b m = if validmove b m
               then putStrLn "Good Move" >> play executemove b m
               else putStrLn "Bad Move" >> play b
