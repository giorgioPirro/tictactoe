module Main exposing (Model, update)

import Html exposing (Html, Attribute, div)
import Html.Attributes exposing (..)
import Task
import Time
import Process

import Msg exposing (Msg(..))
import UI.Board exposing (renderBoard)
import UI.ResetButton exposing (renderResetButton)
import UI.GameOutcome exposing (renderGameOutcome)
import UI.SelectNewGame exposing (renderSelectNewGame, renderSelectNewBoard)
import UI.WhoseTurn exposing (renderWhoseTurn)
import Board exposing (Size(..))
import Game exposing (Game, Status(..))
import Player exposing (Player(..))
import GameGenerator exposing (GameType(..))
import Ai

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = always Sub.none
    }


-- MODEL


type alias Model = { game: Game }

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
                |> updateWithGame

        NewGame boardSize gameType ->
            GameGenerator.createGame boardSize gameType
                |> updateWithGame

updateWithGame : Game -> (Model, Cmd Msg)
updateWithGame game =
    case (Game.whoseTurn game) of
        Just (Human _)    -> ({game = game}, Cmd.none)
        Just (Computer _) -> ({game = game}, (computerMove game))
        Nothing           -> ({game = game}, Cmd.none)

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
        size = Board.size board
        status = Game.status game
        whoseTurn = Game.whoseTurn game
        gameType = GameGenerator.whichGameType (Game.getPlayers game)
    in
        div [class "main-container"]
            [ renderNewGameSelection size gameType status
            , renderBoard status whoseTurn board
            , renderWhoseTurn whoseTurn
            , renderGameOutcome status
            ]

renderNewGameSelection : Size -> GameType -> Status -> Html Msg
renderNewGameSelection size gameType status =
    div [class "new-game-container"]
        [ div
           []
           [ renderSelectNewGame size gameType
           , renderSelectNewBoard size gameType
           ]
        , renderResetButton size gameType status
        ]
