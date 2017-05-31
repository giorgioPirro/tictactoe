module Game exposing(Player(..), Game, Status(..), create, whoseTurn, makeMove,
                     getBoard, status)

import Maybe

import Board exposing(Mark(..), Board, Position)

type Player = Human Mark | Computer Mark
type alias Game = {board: Board, players: (Player, Player)}
type Status = Ongoing | Tie | Win Mark

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

status : Game -> Status
status {board} =
    case (Board.winningMark board) of
        Just mark ->
            Win mark
        Nothing ->
            if (Board.isFull board) then
                Tie
            else
                Ongoing
