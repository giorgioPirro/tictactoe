module Main exposing (Model, renderGameStatus,
                      renderSelectNewGame, renderSelectBoard,
                      renderWhoseTurn, update)

import Html exposing (Html, Attribute, div, text, table, tr, td, p, button,
                      select, option)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Task

import Msg exposing (Msg(..))
import UI.Board exposing (renderBoard)
import UI.ResetButton exposing (renderResetButton)
import Board exposing (Board, Mark(..), Size(..), Position, Cell, size, sizesAvailable,
                       sizeFromString)
import Game exposing (Status(..), Player(..), Game)
import GameGenerator exposing (GameType(..), gameTypes, whichGameType, createGame,
                               gameTypeFromString)
import Ai

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = always Sub.none
    }


-- MODEL


type alias Model = {game: Game}

init : (Model, Cmd Msg)
init =
   let
       defaultGame = GameGenerator.createGame Standard HumanVsHuman
   in
       ({game = defaultGame}, Cmd.none)


-- UPDATE


update : Msg -> Model -> (Model, Cmd Msg)
update msg {game} =
    case msg of
        MakeMove position ->
            Game.makeMove position game
                |> modelWithMessage

        NewGame boardSize gameType ->
            GameGenerator.createGame boardSize gameType
                |> modelWithMessage

modelWithMessage : Game -> (Model, Cmd Msg)
modelWithMessage newGame =
   case (Game.whoseTurn newGame) of
       Just (Human _) -> ({game = newGame}, Cmd.none)
       Just (Computer _) -> ({game = newGame}, (computerMove newGame))
       Nothing -> ({game = newGame}, Cmd.none)

computerMove : Game -> Cmd Msg
computerMove game =
    let
        chosenPosition = Ai.pickBestPosition game
    in
        case chosenPosition of
            Just position ->
                Task.perform MakeMove (Task.succeed position)
            Nothing ->
                Cmd.none


-- VIEW


view : Model -> Html Msg
view {game} =
    let
        board = Game.getBoard game
        size = Board.size board
        gameType = GameGenerator.whichGameType (Game.getPlayers game)
        status = Game.status game
        whoseTurn = Game.whoseTurn game
    in
        div []
            [ renderWhoseTurn whoseTurn
            , renderBoard status whoseTurn board
            , renderResetButton game
            , renderSelectNewGame size gameType
            , renderSelectBoard size gameType
            , renderGameStatus status
            ]

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

renderSelectBoard : Size -> GameType -> Html Msg
renderSelectBoard currentBoardSize currentGameType =
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

renderGameStatus : Status -> Html Msg
renderGameStatus status =
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
