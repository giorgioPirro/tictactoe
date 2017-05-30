module Helpers exposing (createNewGame, createTieGameStandardSizedBoard)

import Board exposing (Size(..), Mark(..))
import Game exposing (Game, Player(..))

createNewGame : Size -> (Player, Player) -> Game
createNewGame size players =
    Game.create players (Board.create size)

createTieGameStandardSizedBoard : Game
createTieGameStandardSizedBoard =
    let
        newGame = Game.create (Human X, Human O) (Board.create Standard)
        moves = [1, 0, 4, 2, 5, 3, 6, 7, 8]
    in
        List.foldl Game.makeMove newGame moves
