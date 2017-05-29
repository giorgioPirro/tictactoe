module Game exposing(Game, create, whoseTurn, makeMove, getBoard)

import Board exposing(Mark(..), Board, Position)

type alias Game = {currentPlayerMark: Mark, board: Board}

create : Mark -> Board -> Game
create mark board =
    Game mark board

whoseTurn : Game -> Mark
whoseTurn {currentPlayerMark} =
    currentPlayerMark

makeMove : Position -> Game -> Game
makeMove position {board, currentPlayerMark} =
    let
        newPlayerMark = switchMark currentPlayerMark
        move = (position, currentPlayerMark)
        newBoard = Board.markCell move board
    in
        Game newPlayerMark newBoard

getBoard : Game -> Board
getBoard {board} =
    board

switchMark : Mark -> Mark
switchMark mark =
    case mark of
        O -> X
        X -> O
