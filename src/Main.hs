module Main where

import Vacuum
import System.Random (randomRIO)

readInt :: String -> IO Int
readInt str = readIO str


getRandomDirection :: Int -> IO Int
getRandomDirection len = randomRIO (0, len-1)


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
    print w
    if n /= 0 then do
        if currentHasDirt (r, w) then
            robotGo (n-1) r (cleanSquare (r, w))
        else do
            if isSameMove (robotMove (r, w)) r then do
                i <- getRandomDirection (length (listDirections r))
                robotGo (n-1) (chooseMoveRandom r i) w
            else
                robotGo (n-1) (robotMove (r, w)) w
    else do
        putStrLn "End"


main :: IO ()
main = do
    n <- getNumMoves
    robotGo n initialPosition initialWorld
