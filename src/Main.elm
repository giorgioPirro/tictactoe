module Main exposing (Msg(..), Model, renderBoard, renderGameStatus, update)

import Html exposing (Html, Attribute, div, text, table, tr, td, p)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Task

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


type alias Model = {game: Game}

init : (Model, Cmd Msg)
init =
    ({game = (Game.create ((Human X),(Computer O)) (Board.create Standard))}, Cmd.none)


-- UPDATE


type Msg = MakeMove Position

update : Msg -> Model -> (Model, Cmd Msg)
update (MakeMove position) {game} =
    let
        updatedGame = Game.makeMove position game
    in
        case (Game.whoseTurnNext game) of
            Just (Human mark) -> ({game = updatedGame}, Cmd.none)
            Just (Computer mark) -> ({game = updatedGame}, Task.perform MakeMove (Task.succeed 1))
            Nothing -> ({game = updatedGame}, Cmd.none)


-- VIEW


type alias Index = Int

view : Model -> Html Msg
view {game} =
    div []
        [ renderBoard (Game.status game) (Game.whoseTurn game) (Game.getBoard game)
        , renderGameStatus (Game.status game)
        ]

renderGameStatus : Status -> Html Msg
renderGameStatus status =
    case status of
        Ongoing ->
            div [class "outcome-box"]
                []
        Tie ->
            div [class "outcome-box"]
                [p [class "outcome-message"] [text "It was a tie!!"]]
        Win mark ->
            div [class "outcome-box"]
                [p [class "outcome-message"] [text ((toString mark) ++ " has won!!")]]

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
    (cell == Nothing) && (isGameOngoing status)

clickEvent : Position -> Cell -> Attribute Msg
clickEvent position cell =
    onClick (MakeMove position)

cellClass : Cell -> String
cellClass cell =
    case cell of
        (Just O) -> "cell noughts"
        (Just X) -> "cell crosses"
        Nothing  -> "cell empty"
