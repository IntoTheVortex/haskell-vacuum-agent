{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
module Vacuum where

    import System.Random (randomRIO)

    data Square where
        Dirt :: Square
        deriving (Eq, Show)

    type Row = (Maybe Square, Maybe Square, Maybe Square)

    data World where
        World :: (Row, Row, Row) -> World
        deriving Eq

    worldToString :: World -> String
    worldToString (World (r0, r1, r2)) =
        "World\n" ++
        " [ " ++ show r0 ++ " ]\n" ++
        " [ " ++ show r1 ++ " ]\n" ++
        " [ " ++ show r2 ++ " ]\n\n"

    instance Show World where show = worldToString

    emptyWorld :: World
    emptyWorld  =
        World
        ( (Nothing, Nothing, Nothing),
         (Nothing, Nothing, Nothing),
         (Nothing, Nothing, Nothing)
        )

    initialWorld :: World
    initialWorld  =
        World
        ( (Just Dirt, Nothing, Just Dirt),
         (Nothing, Just Dirt, Nothing),
         (Just Dirt, Nothing, Just Dirt)
        )

    data RowIndex where
        C0 :: RowIndex
        C1 :: RowIndex
        C2 :: RowIndex
        deriving (Eq, Show)

    indexRow :: (RowIndex, Row) -> Maybe Square
    indexRow (C0, (x0, x1, x2)) = x0
    indexRow (C1, (x0, x1, x2)) = x1
    indexRow (C2, (x0, x1, x2)) = x2

    cleanInLine :: (RowIndex, Row) -> Row
    cleanInLine (C0, (x0, x1, x2)) = (Nothing, x1, x2)
    cleanInLine (C1, (x0, x1, x2)) = (x0, Nothing, x2)
    cleanInLine (C2, (x0, x1, x2)) = (x0, x1, Nothing)

    type WorldIndex = (RowIndex, RowIndex)

    indexWorld :: (WorldIndex, World) -> Maybe Square
    indexWorld ((C0, i), World(r0, r1, r2)) = indexRow (i, r0)
    indexWorld ((C1, i), World(r0, r1, r2)) = indexRow (i, r1)
    indexWorld ((C2, i), World(r0, r1, r2)) = indexRow (i, r2)

    cleanSquare :: (WorldIndex, World) -> World
    cleanSquare ((C0, i), World(r0, r1, r2)) = World (cleanInLine (i, r0), r1, r2)
    cleanSquare ((C1, i), World(r0, r1, r2)) = World (r0, cleanInLine (i, r1), r2)
    cleanSquare ((C2, i), World(r0, r1, r2)) = World (r0, r1, cleanInLine (i, r2))

    type VacuumIndex = (RowIndex, RowIndex)

    initialPosition :: VacuumIndex
    initialPosition = (C1, C1)


    --only get index if there's dirt
    --TODO this will not always be accurate
    getNorthDirt :: (VacuumIndex, World) -> Maybe VacuumIndex
    getNorthDirt ((C0, i), w) = Nothing
    getNorthDirt ((C1, i), w) =
        case indexWorld ((C0, i), w) of
            Just Dirt -> Just (C0, i)
            Nothing -> Nothing
    getNorthDirt ((C2, i), w) =
        case indexWorld ((C1, i), w) of
            Just Dirt -> Just (C1, i)
            Nothing -> Nothing

    getEastDirt :: (VacuumIndex, World) -> Maybe VacuumIndex
    getEastDirt ((i, C0), w) =
        case indexWorld ((i, C1), w) of
            Just Dirt -> Just (i, C1)
            Nothing -> Nothing
    getEastDirt ((i, C1), w) =
        case indexWorld ((i, C2), w) of
            Just Dirt -> Just (i, C2)
            Nothing -> Nothing
    getEastDirt ((i, C2), w) = Nothing

    getSouthDirt :: (VacuumIndex, World) -> Maybe VacuumIndex
    getSouthDirt ((C0, i), w) =
        case indexWorld ((C1, i), w) of
            Just Dirt -> Just (C1, i)
            Nothing -> Nothing
    getSouthDirt ((C1, i), w) =
        case indexWorld ((C2, i), w) of
            Just Dirt -> Just (C2, i)
            Nothing -> Nothing
    getSouthDirt ((C2, i), w) = Nothing

    getWestDirt :: (VacuumIndex, World) -> Maybe VacuumIndex
    getWestDirt ((i, C0), w) = Nothing
    getWestDirt ((i, C1), w) =
        case indexWorld ((i, C0), w) of
            Just Dirt -> Just (i, C0)
            Nothing -> Nothing
    getWestDirt ((i, C2), w) =
        case indexWorld ((i, C1), w) of
            Just Dirt -> Just (i, C1)
            Nothing -> Nothing

    -- checks if the coords fed in are valid. if they are that means dirt
        -- this should be called on results of checkAdj
    makeList :: [Maybe VacuumIndex] -> [VacuumIndex]
    makeList [] = []
    makeList (Just x : xs) = x : makeList xs
    makeList (Nothing : xs) = makeList xs

    --how do i know if valid index?
    checkAdjacent :: (VacuumIndex, World) -> [Maybe VacuumIndex]
    checkAdjacent (v, w) =
        [getNorthDirt (v, w), getEastDirt (v, w), getSouthDirt (v, w), getWestDirt (v, w)]

    cleanCurrentIfDirt :: (VacuumIndex, World) -> World
    cleanCurrentIfDirt  (v, w) =
        case indexWorld (v, w) of
            Just Dirt -> cleanSquare (v, w)
            Nothing -> w

    --instead of cleanCurrentIfDirt
    currentHasDirt :: (VacuumIndex, World) -> Bool
    currentHasDirt (v, w)= 
        case indexWorld (v, w) of
            Just Dirt -> True
            Nothing -> False 



    --if no surrounding indexes have dirt, return the current index
    chooseMoveBasic :: (VacuumIndex, World) -> VacuumIndex
    chooseMoveBasic (v, w) =
        getFromList v (makeList (checkAdjacent (v, w)))

    getFromList :: VacuumIndex -> [VacuumIndex] -> VacuumIndex
    getFromList v l =
        if null l then
            v
        else
            head l

    moveNorth :: VacuumIndex -> Maybe VacuumIndex
    moveNorth (C0, i) = Nothing
    moveNorth (C1, i) = Just (C0, i)
    moveNorth (C2, i) = Just (C1, i)

    moveEast :: VacuumIndex -> Maybe VacuumIndex
    moveEast (i, C0) = Just (i, C1)
    moveEast (i, C1) = Just (i, C2)
    moveEast (i, C2) = Nothing

    moveSouth :: VacuumIndex -> Maybe VacuumIndex
    moveSouth (C0, i) = Just (C1, i)
    moveSouth (C1, i) = Just (C2, i)
    moveSouth (C2, i) = Nothing

    moveWest :: VacuumIndex -> Maybe VacuumIndex
    moveWest (i, C0) = Nothing
    moveWest (i, C1) = Just (i, C0)
    moveWest (i, C2) = Just (i, C1)


    isSameMove :: VacuumIndex -> VacuumIndex -> Bool
    isSameMove v1 v2 =
        v1 == v2


    listDirections :: VacuumIndex -> [VacuumIndex]
    listDirections v = makeList [moveNorth v, moveEast v, moveSouth v, moveWest v]

    chooseMoveRandom :: VacuumIndex -> Int -> VacuumIndex
    chooseMoveRandom v i = 
        listDirections v !! i

    robotMove :: (VacuumIndex, World) -> VacuumIndex
    robotMove (v, w) = chooseMoveBasic (v, w)





