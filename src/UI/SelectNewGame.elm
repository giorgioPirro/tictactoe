module UI.SelectNewGame exposing (renderSelectNewGame, renderSelectNewBoard)

import Html exposing (Html, text, option, select, div)
import Html.Attributes exposing (value, selected)
import Html.Events exposing (onInput)

import Msg exposing (Msg(..))
import Board exposing (Size(..), sizeFromString, sizesAvailable)
import GameGenerator exposing (GameType(..), gameTypeFromString, gameTypes)

renderSelectNewBoard : Size -> GameType -> Html Msg
renderSelectNewBoard currentBoardSize currentGameType =
    case currentGameType of
        HumanVsHuman ->
            sizesAvailable
                |> List.map (\aBoardSize -> (generateOptions currentBoardSize aBoardSize))
                |> select [onInput (generateNewGameFromGivenTypeAndSize currentGameType)]
        _ ->
            div [] []

renderSelectNewGame : Size -> GameType -> Html Msg
renderSelectNewGame boardSize currentGameType =
    gameTypes
        |> List.map (\aGameType -> (generateOptions currentGameType aGameType))
        |> select [onInput generateNewGameFromGivenType]

generateNewGameFromGivenTypeAndSize : GameType -> String -> Msg
generateNewGameFromGivenTypeAndSize currentGameType boardSizeAsString =
    NewGame (sizeFromString boardSizeAsString) currentGameType

generateNewGameFromGivenType : String -> Msg
generateNewGameFromGivenType gameTypeAsString =
    NewGame Standard (gameTypeFromString gameTypeAsString)

generateOptions : a -> a -> Html Msg
generateOptions optionToSelect currentOption =
    let
        typeAsString = toString currentOption
    in
        if (currentOption == optionToSelect) then
            option [value typeAsString, selected True] [text typeAsString]
        else
            option [value typeAsString] [text typeAsString]
