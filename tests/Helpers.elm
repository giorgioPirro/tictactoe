module Helpers exposing (createNewGame)

import Board exposing (Size(..), Mark)
import Game exposing (Game, Player)

createNewGame : Size -> (Player, Player) -> Game
createNewGame size players =
    Game.create players (Board.create size)
