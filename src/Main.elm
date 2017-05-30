module Main exposing (renderBoard)

import Html exposing (Html, div, text, table, tr, td, p)
import Html.Attributes exposing (..)
import Html.Events

import Board exposing (Board, Mark(..), Size(..))

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

type Msg = IrrelevantAtThisPoint


-- VIEW

view : Model -> Html Msg
view model =
    renderBoard (Board.create Standard)

renderBoard : Board -> Html Msg
renderBoard board =
    board
        |> Board.rows
        |> List.map renderRow
        |> table [class "board"]

renderRow : List (Maybe Mark) -> Html Msg
renderRow row =
    row
        |> List.map renderCell
        |> tr [class "row"]

renderCell : Maybe Mark -> Html Msg
renderCell mark =
    td [class (cellClass mark)] [text "I am cell"]

cellClass : Maybe Mark -> String
cellClass mark =
    case mark of
        (Just O) -> "cell noughts"
        (Just X) -> "cell crosses"
        Nothing  -> "cell empty"
