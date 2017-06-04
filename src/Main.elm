module Main exposing (Model, update)

import Html exposing (Html, Attribute, div, text, table, tr, td, p, button,
                      select, option)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Task
import Time
import Process

import Msg exposing (Msg(..))
import UI.Board exposing (renderBoard)
import UI.ResetButton exposing (renderResetButton)
import UI.GameOutcome exposing (renderGameOutcome)
import UI.SelectNewGame exposing (renderSelectNewGame, renderSelectNewBoard)
import UI.WhoseTurn exposing (renderWhoseTurn)
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
        Process.sleep (200 * Time.millisecond)
            |> Task.perform (\() -> getMoveFromAi game)

getMoveFromAi : Game -> Msg
getMoveFromAi game =
    case (Ai.pickBestPosition game) of
        Nothing ->
            MakeMove 0
        Just position ->
           MakeMove position


-- VIEW


view : Model -> Html Msg
view {game} =
    let
        board = Game.getBoard game
        status = Game.status game
        whoseTurn = Game.whoseTurn game
    in
        div [class "main-container"]
            [ renderGameSelection game
            , renderBoard status whoseTurn board
            , renderWhoseTurn whoseTurn
            , renderGameOutcome status
            ]

renderGameSelection : Game -> Html Msg
renderGameSelection game =
    let
        board = Game.getBoard game
        size = Board.size board
        gameType = GameGenerator.whichGameType (Game.getPlayers game)
    in
        div [class "new-game-container"]
            [ div [] [ renderSelectNewGame size gameType
                     , renderSelectNewBoard size gameType
                     ]
            , renderResetButton game
            ]
