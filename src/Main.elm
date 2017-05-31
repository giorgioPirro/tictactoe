module Main exposing (Msg(..), renderBoard)

import Html exposing (Html, div, text, table, tr, td, p)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Board exposing (Board, Mark(..), Size(..), Position)
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

view : Model -> Html Msg
view model =
    renderBoard Ongoing (Human X) (Board.create Standard)

renderBoard : Status -> Player -> Board -> Html Msg
renderBoard status currentPlayer board =
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
    td [class (cellClass mark), (onClick (HumanMove 0))] [text "I am cell"]

cellClass : Maybe Mark -> String
cellClass mark =
    case mark of
        (Just O) -> "cell noughts"
        (Just X) -> "cell crosses"
        Nothing  -> "cell empty"
