module Board exposing (Size(..), Mark(..), create, markCell, toList, toArray)

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

toList : Board -> List (Maybe Mark)
toList (Board marks) =
    Array.toList marks

toArray : Board -> Array (Maybe Mark)
toArray (Board marks) =
    marks
