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
               putStrLn "Whats your name?"
               name_ <- getLine
               putStrLn "Whats your age?"
               age_ <- getLine
               putStrLn "Select difficulty (1-10)"
               skill_ <- getLine
               --let player1 = Player name_ age_
               --let skill = skill_
               playbot initial True

playsetup :: IO()
playsetup = do
          error <- getLine
          putStrLn "Player 1, Please Enter Your Name: "
          player1name <- getLine
          putStrLn "Player 1, Please Enter Your Age: "
          player1age <- getLine
          putStrLn "Player 2, Please Enter Your Name: "
          player2name <- getLine
          putStrLn "Player 2, Please Enter Your Age: "
          player2age <- getLine
          putStrLn $ (player1name ++ " is Player 1 and they are White.")
          putStrLn $ (player2name ++ " is Player 2 and they are Black.")
          play initial True
          --let player1 = Player player1name player1age
          --let player2 = Player player2name player2age



--This function is played repeatedly for each players move
play :: Board -> Bool -> IO()
play b True = do
                printboard b
                putStrLn "It Is Now Whites Turn," --(" ++ player1name ++ ")"
                putStrLn "Enter valid move (a1b2): "
                move <- getLine
                movement (b) (stringtomove(move)) False
play b False = do
                printboard b
                putStrLn "It Is Now Blacks Turn, " --(" ++ player2name ++ ")"
                putStrLn "Enter valid move (a1b2): "
                move <- getLine
                movement (b) (stringtomove(move)) True

playbot :: Board -> Bool -> IO()
playbot b True = do
                printboard b
                if gameover b True then reset' False else do
                      putStrLn "It Is Now Whites Turn,"
                      putStrLn "Enter valid move (a1b2): "
                      move <- getLine
                      movementbot (b) (stringtomove(move))

playbot b False = do
                printboard b
                if gameover b False then reset' True else do
                      playbot (executemove b (getbotmove b)) True

--This function tests if the move is valid and plays it if it is
movement :: Board -> Move -> Bool -> IO()
movement b m x = if validmove b m (not x)
                 then putStrLn "Good Move" >> play (executemove b m) x
                 else putStrLn "Bad Move" >> play b (not x)


movementbot :: Board -> Move -> IO()
movementbot b m = if validmove b m True
               then putStrLn "Good Move" >> playbot (executemove b m) False
               else putStrLn "Bad Move" >> playbot b True

reset' :: Bool -> IO()
reset' True  = do
                putStrLn "White wins! Click (Enter) to start a new game"
                start
reset' False = do
                putStrLn "Black wins! Click (Enter) to start a new game"
                start

gameover :: Board -> Bool -> Bool
gameover b t = (checkmate b t) || (stalemate b t)

--prints a board to stdout
printboard :: Board -> IO()
printboard b = do
               putStrLn ""
               putStrLn "==ABCDEFGH=="
               putStr $ unlines $ zipWith (++)(zipWith (++) ["8 ","7 ","6 ","5 ","4 ","3 ","2 ","1 "] (format $ (showboard b))) [" 8"," 7"," 6"," 5"," 4"," 3"," 2"," 1"]
               putStrLn "==ABCDEFGH=="
               putStrLn ""
