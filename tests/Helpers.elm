module Helpers exposing (createNewGame)

import Board exposing (Size(..), Mark)
import Game exposing (Game)

createNewGame : Size -> Mark -> Game
createNewGame size mark =
    let
        board = Board.create size
    in
        Game.create mark board
