module Board exposing (Size(..), Mark(..), Cell, Board, Position, create, markCell,
                       rows, rowsWithPositions, isFull, isPositionAvailable,
                       winningMark, toList, toArray)

import Array exposing (Array)
import Set

import Utilities.List exposing (removeWhen, findFirstWhere, allItemsAreEqual,
                                transpose, getAt, chunkify)
import Utilities.Maybe exposing (flatMaybe)

type Size  = Standard

type Mark  = X | O
type alias Cell = (Maybe Mark)
type Board = Board (Array Cell)
type alias Line = List Cell
type alias Position = Int
type alias Move = (Position, Mark)

create : Size -> Board
create size =
    case size of
        Standard -> Board (Array.repeat 9 Nothing)

markCell : Move -> Board -> Board
markCell (position, cell) (Board cells) =
    Array.set position (Just cell) cells
        |> Board

isFull : Board -> Bool
isFull (Board marks) =
    Array.toList marks
        |> List.all (\cell -> cell /= Nothing)

isPositionAvailable : Position -> Board -> Bool
isPositionAvailable position (Board cells) =
    Array.get position cells == Just Nothing

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

lineHasEmptyCell : Line -> Bool
lineHasEmptyCell cells =
  List.any (\cell -> cell == Nothing)  cells

allLines : Board -> List Line
allLines board =
    rows board ++ columns board ++ diagonals board

rows : Board -> List Line
rows board =
    toList board
        |> chunkify (width board)

rowsWithPositions : Board -> List (List (Position,Cell))
rowsWithPositions board =
    toList board
        |> List.indexedMap (,)
        |> chunkify (width board)

columns : Board -> List Line
columns board =
    transpose (rows board)

diagonals : Board -> List Line
diagonals board =
    let
        reversedRows = List.reverse (rows board)
    in
        [downDiagonal (rows board), downDiagonal (reversedRows)]

downDiagonal : List Line -> Line
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

toList : Board -> List Cell
toList (Board marks) =
    Array.toList marks

toArray : Board -> Array Cell
toArray (Board marks) =
    marks
