module Main where

import Vacuum

readInt :: String -> IO Int
readInt str = readIO str

getNumMoves :: IO Int 
getNumMoves = do
    putStrLn "Enter a number of moves for the robot vacuum:"
    num <- getLine
    num <- readInt num
    if num < 0 then do
        putStrLn "Enter a positive number."
        getNumMoves
    else do
        putStrLn "Starting!"
        pure num

robotGo :: Int -> VacuumIndex -> World -> IO ()
robotGo n r w = do 
    if n /= 0 then do
        if currentHasDirt (r, w) then
            robotGo (n-1) r (cleanSquare (r, w))
        else
            robotGo (n-1) (robotMove (r, w)) w
    else do
        putStrLn "End"


main :: IO ()
main = do
    n <- getNumMoves
    robotGo n initialPosition initialWorld
