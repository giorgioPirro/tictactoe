module Main exposing (Msg(..), Model, renderBoard, renderGameStatus,
                      renderResetButton, renderSelectNewGame, update)

import Html exposing (Html, Attribute, div, text, table, tr, td, p, button,
                      select, option)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Task

import Board exposing (Board, Mark(..), Size(..), Position, Cell, size)
import Game exposing (Status(..), Player(..), Game)
import GameGenerator exposing (GameType(..), gameTypes, whichGameType)
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
    ({game = (Game.create ((Human X),(Computer O)) (Board.create Standard))}, Cmd.none)


-- UPDATE


type Msg
  = MakeMove Position
  | NewGame Size GameType

update : Msg -> Model -> (Model, Cmd Msg)
update msg {game} =
    case msg of
        MakeMove position ->
            let
                updatedGame = Game.makeMove position game
            in
                case (Game.whoseTurnNext game) of
                    Just (Human mark) -> ({game = updatedGame}, Cmd.none)
                    Just (Computer mark) -> ({game = updatedGame}, (computerMove updatedGame))
                    Nothing -> ({game = updatedGame}, Cmd.none)

        NewGame boardSize gameType ->
            init

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


type alias Index = Int

view : Model -> Html Msg
view {game} =
    div []
        [ renderResetButton game --CHANGEME TO PASS RELEVEANT DATA INSTEAD OF FULL GAME!!!!!!!!!!!!!!!!!
        , renderSelectNewGame (Board.size (.board game)) (whichGameType (.players game))
        , renderBoard (Game.status game) (Game.whoseTurn game) (Game.getBoard game)
        , renderGameStatus (Game.status game)
        ]

renderSelectNewGame : Size -> GameType -> Html Msg
renderSelectNewGame boardSize currentGameType =
    gameTypes
        |> List.map (\aGameType -> (buildOption currentGameType aGameType))
        |> select [onInput (blah boardSize)]

blah : Size -> String -> Msg
blah boardSize gameTypeAsString =
    case gameTypeAsString of
        "HumanVsHuman" ->
            NewGame boardSize HumanVsHuman
        "HumanVsComputer" ->
            NewGame boardSize HumanVsComputer
        "ComputerVsHuman" ->
            NewGame boardSize ComputerVsHuman
        _ ->
            NewGame boardSize ComputerVsComputer

buildOption : GameType -> GameType -> Html Msg
buildOption currentGameType aGameType =
    let
        typeAsString = toString aGameType
    in
        if (aGameType == currentGameType) then
            option [value typeAsString, selected True] [text typeAsString]
        else
            option [value typeAsString] [text typeAsString]

renderResetButton : Game -> Html Msg
renderResetButton {players, board} =
    let
        boardSize = (Board.size board)
    in
        button [onClick (NewGame boardSize (whichGameType players))] [text "Reset"]

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

renderBoard : Status -> Maybe Player -> Board -> Html Msg
renderBoard status currentPlayer board =
    board
        |> Board.rowsWithPositions
        |> List.map (renderRow status)
        |> table [class "board"]

isGameOngoing : Status -> Bool
isGameOngoing status =
    status == Ongoing

renderRow : Status -> List (Position, Cell) -> Html Msg
renderRow status row =
    row
        |> List.map (renderCell status)
        |> tr [class "row"]

renderCell : Status -> (Position, Cell) -> Html Msg
renderCell status indexedCell =
        td (buildCellAttributes status indexedCell) []

buildCellAttributes : Status -> (Position, Cell) -> List (Attribute Msg)
buildCellAttributes status (position, cell) =
    let
        event = (clickEvent position cell)
        klass = class (cellClass cell)
    in
        if (cellShouldHaveMoveEvent status cell) then
            [klass] ++ [event]
        else
            [klass]

cellShouldHaveMoveEvent : Status -> Cell -> Bool
cellShouldHaveMoveEvent status cell =
    (cell == Nothing) && (isGameOngoing status)

clickEvent : Position -> Cell -> Attribute Msg
clickEvent position cell =
    onClick (MakeMove position)

cellClass : Cell -> String
cellClass cell =
    case cell of
        (Just O) -> "cell noughts"
        (Just X) -> "cell crosses"
        Nothing  -> "cell empty"
