module UI.ResetButton exposing (renderResetButton)

import Html exposing (Html, text, button)
import Html.Events exposing (onClick)

import Msg exposing (Msg(..))
import Game exposing (Game)
import Board
import GameGenerator exposing (whichGameType)

renderResetButton : Game -> Html Msg
renderResetButton {players, board} =
    let
        boardSize = (Board.size board)
    in
        button
           [onClick (NewGame boardSize (whichGameType players))]
           [text "Reset"]
