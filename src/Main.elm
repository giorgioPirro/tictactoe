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
        |> List.map renderRow
        |> table [class "board"]

renderRow : List (Position, Cell) -> Html Msg
renderRow row =
    row
        |> List.map renderCell
        |> tr [class "row"]

renderCell : (Position, Cell) -> Html Msg
renderCell (position, cell) =
    td [class (cellClass cell), (clickEvent position cell) ] []

clickEvent : Position -> Cell -> Attribute Msg
clickEvent position cell =
    onClick (HumanMove position)

cellClass : Cell -> String
cellClass cell =
    case cell of
        (Just O) -> "cell noughts"
        (Just X) -> "cell crosses"
        Nothing  -> "cell empty"
