module Main exposing (Msg(..), renderBoard)

import Html exposing (Html, Attribute, div, text, table, tr, td, p)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Board exposing (Board, Mark(..), Size(..), Position, Cell)
import Game exposing (Status(..), Player(..), Game)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = always Sub.none
    }


-- MODEL


type alias Model = Game

init : (Game, Cmd Msg)
init =
    ((Game.create ((Human X),(Human O)) (Board.create Standard)), Cmd.none)


-- UPDATE


type Msg = HumanMove Position

update : Msg -> Model -> (Model, Cmd Msg)
update msg game =
    case msg of
        HumanMove position ->
            ((Game.makeMove position game), Cmd.none)


-- VIEW


type alias Index = Int

view : Game -> Html Msg
view game =
    renderBoard (Game.status game) (Game.whoseTurn game) (Game.getBoard game)

renderBoard : Status -> Maybe Player -> Board -> Html Msg
renderBoard status currentPlayer board =
    board
        |> Board.rowsWithPositions
        |> List.map (renderRow status)
        |> table [class "board"]

isGameOngoing : Status -> Bool
isGameOngoing status =
    status == Ongoing

renderRow : Status -> List (Position, Cell) -> Html Msg
renderRow status row =
    row
        |> List.map (renderCell status)
        |> tr [class "row"]

renderCell : Status -> (Position, Cell) -> Html Msg
renderCell status indexedCell =
        td (buildCellAttributes status indexedCell) [text "a"]

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
    (cell == Nothing) && (isGameOngoing status)

clickEvent : Position -> Cell -> Attribute Msg
clickEvent position cell =
    onClick (HumanMove position)

cellClass : Cell -> String
cellClass cell =
    case cell of
        (Just O) -> "cell noughts"
        (Just X) -> "cell crosses"
        Nothing  -> "cell empty"
