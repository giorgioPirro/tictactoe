module UI.ResetButton exposing (renderResetButton)

import Html exposing (Html, div, button, text)
import Html.Events exposing (onClick)

import Msg exposing (Msg(..))
import Board exposing (Size)
import Game exposing (Status(..))
import GameGenerator exposing (GameType)

renderResetButton : Size -> GameType -> Status -> Html Msg
renderResetButton boardSize gameType gameStatus =
    case gameStatus of
        Ongoing ->
            div [] []
        _ ->
            div [] [button [onClick (NewGame boardSize gameType)] [text "Reset"]]
