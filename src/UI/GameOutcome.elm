module UI.GameOutcome exposing (renderGameOutcome)

import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class)

import Msg exposing (Msg(..))
import Game exposing (Status(..))

renderGameOutcome : Status -> Html Msg
renderGameOutcome status =
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
