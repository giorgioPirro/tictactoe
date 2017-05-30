module Helpers exposing (createNewGame, createTieGameStandardSizedBoard,
                         createDrawBoardStandardSized)

import Board exposing (Size(..), Mark(..), Board)
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

createDrawBoardStandardSized : Board
createDrawBoardStandardSized =
    let
        newBoard = Board.create Standard
        moves = [(1, X), (0, O), (4, X), (2, O),
                 (5, X), (3, O), (6, X), (7, O), (8, X)]
    in
        List.foldl Board.markCell newBoard moves
