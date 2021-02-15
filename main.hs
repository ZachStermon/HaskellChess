import Chess
import AI
import Evaluator
import Types
import Printing
import Movechecker
import Boards

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

start :: IO()
start = do
            e <- getChar
            putStrLn "Play computer or another human? (C,H)"
            gameselection <- getChar
            if gameselection == 'c' || gameselection == 'C' then do playbotsetup
              else do e <- getLine
                      play (makestate initial True)



--HUMAN ONLY GAME
--This function is played repeatedly for each players move
play :: State -> IO()
play s           = do
                printboard (board s)
                if turn s then putStrLn "It Is Now Whites Turn," else putStrLn "It Is Now Blacks Turn, "

                putStrLn "Current evaluation: "
                putStrLn (show (staticeval s))
                putStrLn "Enter valid move (a1b2): "
                move <- getLine
                movement s (stringtomove(move))

--This function tests if the move is valid and plays it if it is
movement :: State -> Move -> IO()
movement s m = if validmove s m
                 then putStrLn "Good Move" >> play (domove s m)
                 else putStrLn "Bad Move" >> play s


--BOT GAME
playbotsetup :: IO()
playbotsetup = do
               e <- getLine
               putStrLn "Select difficulty (1-10)"
               skill <- getLine
               putStrLn "Play white or black? (W,B)"
               color <- getChar
               e <- getLine
               if color == 'w' || color == 'W'
               then playbotw (makestate initial True)
               else playbotb (makestate initial True)





playbotw :: State -> IO()
--turn is white
playbotw s | turn s = do
                printboard (board s)
                if gameover s then reset' False else do
                      putStrLn "It Is Now Whites Turn,"
                      putStrLn "Current evaluation: "
                      putStrLn (show (staticeval s))
                      putStrLn "Enter valid move (a1b2): "
                      move <- getLine
                      movementbotw s (stringtomove(move))
--turn is black
playbotw s         = do
                printboard (board s)
                putStrLn "Current evaluation: "
                putStrLn (show (staticeval s))
                if gameover s then reset' True else do
                      let m = findbestmove s
                      putStrLn "Bot played:"
                      putStrLn (show m)
                      playbotw (domove s m)

playbotb :: State -> IO()
--turn is white
playbotb s | turn s = do
                printboard (board s)
                putStrLn "Current evaluation: "
                putStrLn (show (staticeval s))
                if gameover s then reset' True else do
                      let m = findbestmove s
                      putStrLn "Bot played:"
                      putStrLn (show m)
                      playbotb (domove s m)
--turn is black
playbotb s           = do
                printboard (board s)
                if gameover s then reset' False else do
                      putStrLn "It Is Now Blacks Turn,"
                      putStrLn "Current evaluation: "
                      putStrLn (show (staticeval s))
                      putStrLn "Enter valid move (a1b2): "
                      move <- getLine
                      movementbotb s (stringtomove(move))


movementbotb :: State -> Move -> IO()
movementbotb s m = if validmove s m
               then putStrLn "Good Move" >> playbotb (domove s m)
               else putStrLn "Bad Move" >> playbotb s

movementbotw :: State -> Move -> IO()
movementbotw s m = if validmove s m
               then putStrLn "Good Move" >> playbotw (domove s m)
               else putStrLn "Bad Move" >> playbotw s



--for reseting the game
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
