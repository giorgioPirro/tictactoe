module Game exposing(Player(..), Game, create, newCreate, whoseTurn, newWhoseTurn, makeMove, getBoard)

import Maybe

import Board exposing(Mark(..), Board, Position)

type Player = Human Mark | Computer Mark
type alias Game = {currentPlayerMark: Mark, board: Board, players: (Player, Player)}

create : Mark -> Board -> Game
create mark board =
    Game mark board (Human X, Human O)

newCreate : (Player, Player) -> Board -> Game
newCreate players board =
    Game X board players

whoseTurn : Game -> Mark
whoseTurn {currentPlayerMark} =
    currentPlayerMark

newWhoseTurn : Game -> Maybe Player
newWhoseTurn {players} =
    (Just (Tuple.first players))

makeMove : Position -> Game -> Game
makeMove position {board, currentPlayerMark, players} =
    let
        mARRRKOfcurrentPlayer = extractMark(Tuple.first players)
        newPlayerMark = switchMark currentPlayerMark
        move = (position, mARRRKOfcurrentPlayer)
        newBoard = Board.markCell move board
    in
        Game newPlayerMark newBoard (Tuple.second players, Tuple.first players)

getBoard : Game -> Board
getBoard {board} =
    board

switchMark : Mark -> Mark
switchMark mark =
    case mark of
        O -> X
        X -> O

extractMark : Player -> Mark
extractMark player =
    case player of
        Human mark -> mark
        Computer mark -> mark
