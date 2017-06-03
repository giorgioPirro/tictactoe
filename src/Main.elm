module Main exposing (Model, update)

import Html exposing (Html, Attribute, div, text, table, tr, td, p, button,
                      select, option)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Task

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
            , renderSelectNewBoard size gameType
            , renderGameOutcome status
            ]
