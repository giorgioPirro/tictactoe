module UI.Board exposing (renderBoard)

import Html exposing (Html, Attribute, div, table, tr, td)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Msg exposing (Msg(..))
import Game exposing (Status(..))
import Player exposing (Player(..))
import Board exposing (Board, Position, Cell, Mark(..))

type alias IndexedCell = (Position, Cell)

renderBoard : Status -> Maybe Player -> Board -> Html Msg
renderBoard status currentPlayer board =
    let
        renderedBoard =
            board
              |> Board.rowsWithPositions
              |> List.map (renderRow status currentPlayer)
              |> table [class "board"]
    in
        div [class "board-container"] [renderedBoard]

renderRow : Status -> Maybe Player -> List (Position, Cell) -> Html Msg
renderRow status currentPlayer row =
    row
        |> List.map (renderCell status currentPlayer)
        |> tr [class "row"]

renderCell : Status -> Maybe Player -> IndexedCell -> Html Msg
renderCell status currentPlayer indexedCell =
        td (buildCellAttributes status currentPlayer indexedCell) []

buildCellAttributes : Status -> Maybe Player -> IndexedCell -> List (Attribute Msg)
buildCellAttributes status currentPlayer (position, cell) =
    let
        event = (clickEvent position cell)
        klass = class (cellClass cell)
    in
        if (cellShouldHaveMoveEvent status cell currentPlayer) then
            [klass] ++ [event]
        else
            [klass]

clickEvent : Position -> Cell -> Attribute Msg
clickEvent position cell =
    onClick (MakeMove position)

cellClass : Cell -> String
cellClass cell =
    case cell of
        (Just O) -> "cell noughts"
        (Just X) -> "cell crosses"
        Nothing  -> "cell empty"

cellShouldHaveMoveEvent : Status -> Cell -> Maybe Player -> Bool
cellShouldHaveMoveEvent status cell currentPlayer =
    case currentPlayer of
        Nothing ->
            False
        Just (Human _) ->
            (cell == Nothing) && (not (Game.gameIsOver status))
        Just (Computer _) ->
            False
