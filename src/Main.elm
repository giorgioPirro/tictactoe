module Main exposing (renderBoard)

import Html exposing (Html, div, text, table, tr, td, p)
import Html.Attributes exposing (..)
import Html.Events

import Board exposing (Board)

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
    table [] [tr [] [cell, cell], tr [] [cell, cell]]

renderBoard : Board -> Html Msg
renderBoard board =
  div [class "board"] [cell, cell, cell, cell, cell, cell, cell, cell, cell]

cell : Html Msg
cell = td [class "cell"] [text "I am cell"]
