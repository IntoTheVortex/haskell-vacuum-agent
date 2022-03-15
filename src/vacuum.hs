module Vacuum where

    data Square where
        Dirt :: Square
        Clean :: Square
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

    cleanSquare :: (RowIndex, Row) -> Row
    cleanSquare (C0, (x0, x1, x2)) = (Nothing, x1, x2) 
    cleanSquare (C1, (x0, x1, x2)) = (x0, Nothing, x2) 
    cleanSquare (C2, (x0, x1, x2)) = (x0, x1, Nothing) 

    



