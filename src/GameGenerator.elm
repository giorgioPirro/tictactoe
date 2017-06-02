module GameGenerator exposing (GameType(..), gameTypes, whichGameType)

import Game exposing (Player(..))
import Board exposing (Board, Size(..))

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
