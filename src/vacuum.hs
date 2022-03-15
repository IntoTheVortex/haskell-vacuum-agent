module Vacuum where

    data Square where
        Dirt :: Square
        --Clean :: Square
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
    checkForDirt (v, w) =
        --???
        Nothing, _ -> False
        case indexWorld (Just v, w) of
            Nothing -> False
            Just Dirt -> True

    --how do i know if valid index?
    checkAdjacent :: (VacuumIndex, World) -> [Bool]
    checkAdjacent (v, w) = undefined
    -- fold checkfordirt over every adj, what about Nothing?
        --1 N
        --2 E
        --3 S
        --4 W

    getNorth :: VacuumIndex -> Maybe VacuumIndex
    getNorth (C0, i) = Nothing
    getNorth (C1, i) = Just (C0, i) 
    getNorth (C2, i) = Just (C1, i) 

    getEast :: VacuumIndex -> Maybe VacuumIndex
    getEast (i, C0) = Just (i, C1) 
    getEast (i, C1) = Just (i, C2) 
    getEast (i, C2) = Nothing

    getSouth:: VacuumIndex -> Maybe VacuumIndex
    getSouth (C0, i) = Just (C1, i) 
    getSouth (C1, i) = Just (C2, i) 
    getSouth (C2, i) = Nothing

    getWest :: VacuumIndex -> Maybe VacuumIndex
    getWest (i, C0) = Nothing
    getWest (i, C1) = Just (i, C1) 
    getWest (i, C2) = Just (i, C2)



    
