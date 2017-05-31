module Main exposing (Msg(..), renderBoard)

import Html exposing (Html, Attribute, div, text, table, tr, td, p)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Board exposing (Board, Mark(..), Size(..), Position, Cell)
import Game exposing (Status(..), Player(..))

main =
  Html.program
    { init = (0, Cmd.none)
    , view = view
    , update = \msg model -> (0, Cmd.none)
    , subscriptions = always Sub.none
    }


-- MODEL


type alias Model = Int


-- UPDATE

type Msg = HumanMove Position


-- VIEW

type alias Index = Int

view : Model -> Html Msg
view model =
    renderBoard Ongoing (Human X) (Board.create Standard)

renderBoard : Status -> Player -> Board -> Html Msg
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
    td [class (cellClass cell), (clickEvent position cell) ] [text "I am cell"]

clickEvent : Position -> Cell -> Attribute Msg
clickEvent position cell =
    onClick (HumanMove position)

cellClass : Cell -> String
cellClass cell =
    case cell of
        (Just O) -> "cell noughts"
        (Just X) -> "cell crosses"
        Nothing  -> "cell empty"
