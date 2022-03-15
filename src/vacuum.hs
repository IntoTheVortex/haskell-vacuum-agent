module Vacuum where

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

    {- TODO
    createRandomList :: Int -> [IO Int]
    createRandomList x = do
        case x of
            0 -> []
        -- return 1 or 0 : result from rec call
        -}



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

    --TODO this will not always be accurate
    checkForDirt :: (Maybe VacuumIndex, World) -> Bool
    checkForDirt (v, w) = undefined

    --how do i know if valid index?
    --checkAdjacent :: (VacuumIndex, World) -> [Bool]
    checkAdjacent :: (VacuumIndex, World) -> [Maybe VacuumIndex]
    checkAdjacent (v, w) =
        [getNorth (v, w), getEast (v, w), getSouth (v, w), getWest (v, w)]
    --checkAdjacent (v, w) = undefined
    -- fold checkfordirt over every adj, what about Nothing?
        --1 N --2 E --3 S --4 W

    --only get index if there's dirt
    getNorth :: (VacuumIndex, World) -> Maybe VacuumIndex
    getNorth ((C0, i), w) = Nothing
    getNorth ((C1, i), w) =
        case indexWorld ((C0, i), w) of
            Just Dirt -> Just (C0, i)
            Nothing -> Nothing
    getNorth ((C2, i), w) = 
        case indexWorld ((C1, i), w) of
            Just Dirt -> Just (C1, i)
            Nothing -> Nothing

    getEast :: (VacuumIndex, World) -> Maybe VacuumIndex
    getEast ((i, C0), w) = 
        case indexWorld ((i, C1), w) of
            Just Dirt -> Just (i, C1)
            Nothing -> Nothing
    getEast ((i, C1), w) = 
        case indexWorld ((i, C2), w) of
            Just Dirt -> Just (i, C2)
            Nothing -> Nothing
    getEast ((i, C2), w) = Nothing

    getSouth :: (VacuumIndex, World) -> Maybe VacuumIndex
    getSouth ((C0, i), w) = 
        case indexWorld ((C1, i), w) of
            Just Dirt -> Just (C1, i)
            Nothing -> Nothing
    getSouth ((C1, i), w) = 
        case indexWorld ((C2, i), w) of
            Just Dirt -> Just (C2, i)
            Nothing -> Nothing
    getSouth ((C2, i), w) = Nothing

    getWest :: (VacuumIndex, World) -> Maybe VacuumIndex
    getWest ((i, C0), w) = Nothing
    getWest ((i, C1), w) = 
        case indexWorld ((i, C0), w) of
            Just Dirt -> Just (i, C0)
            Nothing -> Nothing
    getWest ((i, C2), w) = 
        case indexWorld ((i, C1), w) of
            Just Dirt -> Just (i, C1)
            Nothing -> Nothing




