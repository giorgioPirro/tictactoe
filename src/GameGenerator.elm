module GameGenerator exposing (GameType(..))

import Game exposing (Player(..))
import Board exposing (Board)

type GameType
  = HumanVsHuman ((Human, Human) Board)
  | HumanVsComputer ((Human, Computer) Board)
  | ComputerVsComputer ((Computer, Computer) Board)
