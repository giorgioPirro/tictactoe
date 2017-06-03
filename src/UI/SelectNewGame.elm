module UI.SelectNewGame exposing (renderSelectNewGame, renderSelectNewBoard)

import Html exposing (Html, text, option, select)
import Html.Attributes exposing (value, selected)
import Html.Events exposing (onInput)

import Msg exposing (Msg(..))
import Board exposing (Size, sizeFromString, sizesAvailable)
import GameGenerator exposing (GameType, gameTypeFromString, gameTypes)

renderSelectNewBoard : Size -> GameType -> Html Msg
renderSelectNewBoard currentBoardSize currentGameType =
    sizesAvailable
        |> List.map (\aBoardSize -> (buildOption currentBoardSize aBoardSize))
        |> select [onInput (fooCHANGEMYNAMECHANGEMYNAMEPLEASE currentGameType)]

fooCHANGEMYNAMECHANGEMYNAMEPLEASE : GameType -> String -> Msg
fooCHANGEMYNAMECHANGEMYNAMEPLEASE currentGameType boardSizeAsString =
    NewGame (sizeFromString boardSizeAsString) currentGameType

renderSelectNewGame : Size -> GameType -> Html Msg
renderSelectNewGame boardSize currentGameType =
    gameTypes
        |> List.map (\aGameType -> (buildOption currentGameType aGameType))
        |> select [onInput (generateNewGameMsg boardSize)]

generateNewGameMsg : Size -> String -> Msg
generateNewGameMsg boardSize gameTypeAsString =
    NewGame boardSize (gameTypeFromString gameTypeAsString)

buildOption : a -> a -> Html Msg
buildOption optionToSelect currentOption =
    let
        typeAsString = toString currentOption
    in
        if (currentOption == optionToSelect) then
            option [value typeAsString, selected True] [text typeAsString]
        else
            option [value typeAsString] [text typeAsString]
