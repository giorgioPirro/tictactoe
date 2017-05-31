module Board exposing (Size(..), Mark(..), Board, Position, create, markCell,
                       rows, isFull, winningMark, toList, toArray)

import Array exposing (Array)
import Set

import Utilities.List exposing (removeWhen, findFirstWhere, allItemsAreEqual,
                                transpose, getAt)
import Utilities.Maybe exposing (flatMaybe)

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

chunkify : Int -> List a -> List (List a)
chunkify chunkSize list =
    case (List.take chunkSize list) of
        [] -> []
        chunk -> chunk :: (chunkify chunkSize (List.drop chunkSize list))

isFull : Board -> Bool
isFull (Board marks) =
    Array.toList marks
        |> List.all (\mark -> mark /= Nothing)

winningMark : Board -> Maybe Mark
winningMark board =
    let
        winningLine =
            board
                |> allLines
                |> removeWhen lineHasEmptyCell
                |> findFirstWhere allItemsAreEqual
    in
        case winningLine of
            Nothing -> Nothing
            Just line -> flatMaybe (List.head line)

lineHasEmptyCell : List (Maybe Mark) -> Bool
lineHasEmptyCell cells =
  List.any (\cell -> cell == Nothing)  cells

allLines : Board -> List (List (Maybe Mark))
allLines board =
    rows board ++ columns board ++ diagonals board

rows : Board -> List (List (Maybe Mark))
rows ((Board marks) as board) =
    let
        cells = Array.toList marks
    in
        chunkify (width board) cells

columns : Board -> List (List (Maybe Mark))
columns board =
    transpose (rows board)

diagonals : Board -> List (List (Maybe Mark))
diagonals board =
    let
        reversedRows = List.reverse (rows board)
    in
        [downDiagonal (rows board), downDiagonal (reversedRows)]

downDiagonal : List (List (Maybe Mark)) -> List (Maybe Mark)
downDiagonal rows =
    List.indexedMap (,) rows
        |> List.filterMap (\(index, row) -> (getAt row index))

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
