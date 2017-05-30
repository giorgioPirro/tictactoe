module Game exposing(Player(..), Game, create, whoseTurn, makeMove, getBoard)

import Maybe

import Board exposing(Mark(..), Board, Position)

type Player = Human Mark | Computer Mark
type alias Game = {board: Board, players: (Player, Player)}

create : (Player, Player) -> Board -> Game
create players board =
    Game board players

whoseTurn : Game -> Maybe Player
whoseTurn {players} =
    (Just (Tuple.first players))

makeMove : Position -> Game -> Game
makeMove position {board, players} =
    let
        markOfCurrentPlayer = extractMark(Tuple.first players)
        move = (position, markOfCurrentPlayer)
        newBoard = Board.markCell move board
    in
        Game newBoard (swapPlayers players)

getBoard : Game -> Board
getBoard {board} =
    board

extractMark : Player -> Mark
extractMark player =
    case player of
        Human mark -> mark
        Computer mark -> mark

swapPlayers : (Player, Player) -> (Player, Player)
swapPlayers (playerOne, playerTwo) =
    (playerTwo, playerOne)
