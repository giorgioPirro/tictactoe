module UI.Board exposing (renderBoard)

import Html exposing (Html, Attribute, table, tr, td)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Msg exposing (Msg(..))
import Game exposing (Status(..), Player(..))
import Board exposing (Board, Position, Cell, Mark(..))

renderBoard : Status -> Maybe Player -> Board -> Html Msg
renderBoard status currentPlayer board =
    board
        |> Board.rowsWithPositions
        |> List.map (renderRow status)
        |> table [class "board"]

renderRow : Status -> List (Position, Cell) -> Html Msg
renderRow status row =
    row
        |> List.map (renderCell status)
        |> tr [class "row"]

renderCell : Status -> (Position, Cell) -> Html Msg
renderCell status indexedCell =
        td (buildCellAttributes status indexedCell) []

buildCellAttributes : Status -> (Position, Cell) -> List (Attribute Msg)
buildCellAttributes status (position, cell) =
    let
        event = (clickEvent position cell)
        klass = class (cellClass cell)
    in
        if (cellShouldHaveMoveEvent status cell) then
            [klass] ++ [event]
        else
            [klass]

cellShouldHaveMoveEvent : Status -> Cell -> Bool
cellShouldHaveMoveEvent status cell =
    (cell == Nothing) && (not (Game.gameIsOver status))

clickEvent : Position -> Cell -> Attribute Msg
clickEvent position cell =
    onClick (MakeMove position)

cellClass : Cell -> String
cellClass cell =
    case cell of
        (Just O) -> "cell noughts"
        (Just X) -> "cell crosses"
        Nothing  -> "cell empty"
