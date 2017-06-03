module UI.WhoseTurn exposing (renderWhoseTurn)

import Html exposing (Html, div)
import Html.Attributes exposing (class, id)

import Msg exposing (Msg(..))
import Board exposing (Mark(..))
import Game exposing (Player)

renderWhoseTurn : Maybe Player -> Html Msg
renderWhoseTurn player =
    case player of
        Nothing ->
            div []
                [ div [id "turn-crosses", class "turn-display"] []
                , div [id "turn-noughts", class "turn-display"] []
                ]
        Just player ->
            div [] (buildTurnBoxes (Game.extractMark player))

buildTurnBoxes : Mark -> List (Html Msg)
buildTurnBoxes mark =
    case mark of
        X ->
            [ div [id "turn-crosses", class "turn-display turn-current"] []
            , div [id "turn-noughts", class "turn-display"] []
            ]
        O ->
            [ div [id "turn-crosses", class "turn-display"] []
            , div [id "turn-noughts", class "turn-display turn-current"] []
            ]
