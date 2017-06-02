module GameGenerator exposing (GameType(..), gameTypes, whichGameType, createGame,
                               gameTypeFromString)

import Game exposing (Game, Player(..))
import Board exposing (Board, Size(..), Mark(..))

type GameType
  = HumanVsHuman
  | HumanVsComputer
  | ComputerVsHuman
  | ComputerVsComputer

gameTypes : List GameType
gameTypes =
    [HumanVsHuman, HumanVsComputer, ComputerVsHuman, ComputerVsComputer]

whichGameType : (Player, Player) -> GameType
whichGameType players =
    case players of
        (Human _, Human _) ->
            HumanVsHuman
        (Human _, Computer _) ->
            HumanVsComputer
        (Computer _, Human _) ->
            ComputerVsHuman
        (Computer _, Computer _) ->
            ComputerVsComputer

createGame : Size -> GameType -> Game
createGame boardSize gameType =
    let
        board = Board.create boardSize
    in
      case gameType of
          HumanVsHuman ->
              Game.create (Human X, Human O) board
          HumanVsComputer ->
              Game.create (Human X, Computer O) board
          ComputerVsHuman ->
              Game.create (Computer X, Human O) board
          ComputerVsComputer ->
              Game.create (Computer X, Computer O) board

gameTypeFromString : String -> GameType
gameTypeFromString typeDescription =
    case typeDescription of
        "HumanVsHuman" ->
            HumanVsHuman
        "HumanVsComputer" ->
            HumanVsComputer
        "ComputerVsHuman" ->
            ComputerVsHuman
        _ ->
            ComputerVsComputer

