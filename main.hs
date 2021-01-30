import Chess

--This function creates the initial chess board position
--Then the function calls the main play function
main :: IO()
main = do
       putStrLn ""
       putStrLn "Authors: Jacob Swisher and Zach Stermon"
       putStrLn "Version: 1.2"
       putStrLn ""
       putStrLn "This is a simple chess program written entirely in haskell"
       putStrLn "To play a move, type in the origin square (eg. `e2`), followed"
       putStrLn "immediately by the destination square (eg. `e2e4` moves the piece"
       putStrLn "in e2 to the square e4)"
       putStrLn ""
       putStrLn "This program uses FEN ASCII notation to represent chess pieces"
       putStrLn "Uppercase letters are white, lowercase are black"
       putStrLn ""
       putStrLn "If you are still confused, type (I) for more information, otherwise type (G) to select your game type: "
       moreinfo <- getChar
       if moreinfo == 'i' || moreinfo == 'I'
       then info
       else start



start :: IO()
start = do
            error <- getChar
            putStrLn "Play computer or another human? (C,H)"
            gameselection <- getChar
            if gameselection == 'c' || gameselection == 'C'
            then playbotsetup
            else playsetup

info :: IO()
info = do
       putStrLn ""
       putStrLn "More information on FEN notation:"
       putStrLn "Uppercase letters are white, lowercase are black"
       putStrLn "P -> White Pawn"
       putStrLn "N -> White Knight"
       putStrLn "B -> White Bishop"
       putStrLn "R -> White Rook"
       putStrLn "Q -> White Queen"
       putStrLn "K -> White King"
       putStrLn "p -> Black Pawn"
       putStrLn "n -> Black Knight"
       putStrLn "b -> Black Bishop"
       putStrLn "r -> Black Rook"
       putStrLn "q -> Black Queen"
       putStrLn "k -> Black King"
       putStrLn ""
       start




playbotsetup :: IO()
playbotsetup = do
               error <- getLine
               putStrLn "Select difficulty (1-10)"
               skill <- getLine
               putStrLn "Play white or black? (W,B)"
               color <- getChar
               error <- getLine
               if color == 'w' || color == 'W'
               then playbotw initial True
               else playbotb initial True

playsetup :: IO()
playsetup = do
          error <- getLine
          play initial True



--This function is played repeatedly for each players move
play :: Board -> Bool -> IO()
play b True = do
                printboard b
                putStrLn "It Is Now Whites Turn,"
                putStrLn "Enter valid move (a1b2): "
                move <- getLine
                movement (b) (stringtomove(move)) False
play b False = do
                printboard b
                putStrLn "It Is Now Blacks Turn, "
                putStrLn "Enter valid move (a1b2): "
                move <- getLine
                movement (b) (stringtomove(move)) True

--This function tests if the move is valid and plays it if it is
movement :: Board -> Move -> Bool -> IO()
movement b m x = if validmove b m (not x)
                 then putStrLn "Good Move" >> play (executemove b m) x
                 else putStrLn "Bad Move" >> play b (not x)

playbotw :: Board -> Bool -> IO()
playbotw b True = do
                printboard b
                if gameover b True then reset' False else do
                      putStrLn "It Is Now Whites Turn,"
                      putStrLn "Enter valid move (a1b2): "
                      move <- getLine
                      movementbotw b (stringtomove(move))

playbotw b False = do
                printboard b
                if gameover b False then reset' True else do
                      let m = getbotmove b False
                      playbotw (executemove b m) True
                      putStrLn "Bot played:"
                      putStrLn (show m)

playbotb :: Board -> Bool -> IO()
playbotb b True = do
                printboard b
                if gameover b False then reset' True else do
                      let m = getbotmove b True
                      playbotb (executemove b m) False
                      putStrLn "Bot played:"
                      putStrLn (show m)
playbotb b False = do
                printboard b
                if gameover b True then reset' False else do
                      putStrLn "It Is Now Blacks Turn,"
                      putStrLn "Enter valid move (a1b2): "
                      move <- getLine
                      movementbotb b (stringtomove(move))


movementbotb :: Board -> Move -> IO()
movementbotb b m = if validmove b m False
               then putStrLn "Good Move" >> playbotb (executemove b m) True
               else putStrLn "Bad Move" >> playbotb b False

movementbotw :: Board -> Move -> IO()
movementbotw b m = if validmove b m True
               then putStrLn "Good Move" >> playbotw (executemove b m) False
               else putStrLn "Bad Move" >> playbotw b True

reset' :: Bool -> IO()
reset' True  = do
                putStrLn "White wins! Click (Enter) to start a new game"
                start
reset' False = do
                putStrLn "Black wins! Click (Enter) to start a new game"
                start


--prints a board to stdout
printboard :: Board -> IO()
printboard b = do
                putStrLn (boardtostring b)
