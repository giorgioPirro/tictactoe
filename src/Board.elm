module Board exposing (Size(..), Mark(..), Board, Position, create, markCell,
                       rows, toList, toArray)

import Array exposing (Array)

type Size  = Standard
type Mark  = X | O
type Board = Board (Array (Maybe Mark))
type alias Position = Int
type alias Move = (Position, Mark)

create : Size -> Board
create size =
    case size of
        Standard -> Board (Array.repeat 9 Nothing)

markCell : Move -> Board -> Board
markCell (position, mark) (Board cells) =
    Array.set position (Just mark) cells
        |> Board

rows : Board -> List (List (Maybe Mark))
rows ((Board marks) as board) =
    let
        cells = Array.toList marks
    in
        chunkify (width board) cells

chunkify : Int -> List a -> List (List a)
chunkify chunkSize list =
    case (List.take chunkSize list) of
        [] -> []
        chunk -> chunk :: (chunkify chunkSize (List.drop chunkSize list))

width : Board -> Int
width (Board marks) =
    marks
        |> Array.length
        |> toFloat
        |> sqrt
        |> truncate

toList : Board -> List (Maybe Mark)
toList (Board marks) =
    Array.toList marks

toArray : Board -> Array (Maybe Mark)
toArray (Board marks) =
    marks
