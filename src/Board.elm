module Board exposing (Size(..), create, toList)

import Array exposing (Array)

type Size  = Standard
type Mark  = X | O
type Board = Board (Array (Maybe Mark))

create : Size -> Board
create size =
    case size of
        Standard -> Board (Array.repeat 9 Nothing)

toList : Board -> List (Maybe Mark)
toList (Board marks) =
    Array.toList marks
