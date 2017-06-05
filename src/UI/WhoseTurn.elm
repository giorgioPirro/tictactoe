module UI.WhoseTurn exposing (renderWhoseTurn)

import Html exposing (Html, div)
import Html.Attributes exposing (class, id)

import Msg exposing (Msg)
import Board exposing (Mark(..))
import Player exposing (Player)

renderWhoseTurn : Maybe Player -> Html Msg
renderWhoseTurn player =
    case player of
        Nothing ->
            div [class "turn-container"]
                [ div [id "turn-crosses", class "turn-display"] []
                , div [id "turn-noughts", class "turn-display"] []
                ]
        Just player ->
            div [class "turn-container"] (buildTurnBoxes (Player.extractMark player))

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
