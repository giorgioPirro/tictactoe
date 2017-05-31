module Helpers exposing (createNewGame, createTieGameStandardSizedBoard,
                         createDrawBoardStandardSized, standardBoardXwinsHorizontally,
                         standardBoardOwinsHorizontally, standardBoardXwinsVertically,
                         standardBoardOwinsVertically, standardBoardXwinsDownDiagonal,
                         standardBoardOwinsUpDiagonal)

import Board exposing (Size(..), Mark(..), Board)
import Game exposing (Game, Player(..))

createNewGame : Size -> (Player, Player) -> Game
createNewGame size players =
    Game.create players (Board.create size)


addMovesToBoard : List (Int, Mark) -> Board -> Board
addMovesToBoard moves board =
        List.foldl Board.markCell board moves

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

standardBoardXwinsHorizontally : Board
standardBoardXwinsHorizontally =
    let
        moves = [(0, X), (3, O), (1, X), (4, O), (2, X)]
    in
        addMovesToBoard moves (Board.create Standard)

standardBoardOwinsHorizontally : Board
standardBoardOwinsHorizontally =
    let
        moves = [(6, O), (0, X), (7, O), (1, X), (8, O)]
    in
        addMovesToBoard moves (Board.create Standard)

standardBoardXwinsVertically : Board
standardBoardXwinsVertically =
    let
        moves = [(0, X), (1, O), (3, X), (2, O), (6, X)]
    in
        addMovesToBoard moves (Board.create Standard)

standardBoardOwinsVertically : Board
standardBoardOwinsVertically =
    let
        moves = [(1, O), (0, X), (4, O), (3, X), (7, O)]
    in
        addMovesToBoard moves (Board.create Standard)

standardBoardXwinsDownDiagonal : Board
standardBoardXwinsDownDiagonal =
    let
        moves = [(0, X), (1, O), (4, X), (5, O), (8, X)]
    in
        addMovesToBoard moves (Board.create Standard)

standardBoardOwinsUpDiagonal : Board
standardBoardOwinsUpDiagonal =
    let
        moves = [(6, O), (0, X), (4, O), (5, X), (2, O), (8, X)]
    in
        addMovesToBoard moves (Board.create Standard)
