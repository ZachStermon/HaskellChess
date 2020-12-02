import board

--This function creates the initial chess board position
--Then the function calls the main play function
main :: IO()
main = play init

--This function is played repeatedly for each players move
play :: Board -> IO()
play b = do print b
                putStr "move(ORIGIN DESTINATION): "
                move <- getLine
                movement stringtomove(move) b


--This function tests if the move is valid and plays it if it is
movement :: Board -> move -> IO()
movement b m = if validmove m
               then putStrLn "Good Move" >> play executemove b m
               else putStrLn "Bad Move" >> play b
