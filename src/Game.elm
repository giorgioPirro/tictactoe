module Game exposing(Player(..), Game, Status(..), create, whoseTurn, makeMove,
                     getBoard, status)

import Maybe

import Board exposing(Mark(..), Board, Position, Move)

type Player = Human Mark | Computer Mark
type alias Game = {board: Board, players: (Player, Player)}
type Status = Ongoing | Tie | Win Mark

create : (Player, Player) -> Board -> Game
create players board =
    Game board players

whoseTurn : Game -> Maybe Player
whoseTurn ({players} as game) =
    case (status game) of
        Ongoing ->
            (Just (Tuple.first players))
        _ ->
            Nothing

makeMove : Position -> Game -> Game
makeMove position game =
    if (isMoveAllowed position game) then
        addMoveToGame position game
    else
        game

isMoveAllowed : Position -> Game -> Bool
isMoveAllowed position {board} =
    Board.isPositionAvailable position board

addMoveToGame : Position -> Game -> Game
addMoveToGame position {board, players} =
    let
        move = (position, (currentPlayersMark players))
        newBoard = addMoveToBoard move board
        newPlayers = swapPlayers players
    in
        Game newBoard newPlayers

addMoveToBoard : Move -> Board -> Board
addMoveToBoard move board =
    Board.markCell move board

swapPlayers : (Player, Player) -> (Player, Player)
swapPlayers (playerOne, playerTwo) =
    (playerTwo, playerOne)

currentPlayersMark : (Player, Player) -> Mark
currentPlayersMark (currentPlayer, _) =
    extractMark currentPlayer

extractMark : Player -> Mark
extractMark player =
    case player of
        Human mark -> mark
        Computer mark -> mark


getBoard : Game -> Board
getBoard {board} =
    board

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
