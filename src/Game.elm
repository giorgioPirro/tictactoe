module Game exposing(Game, Status(..), create, whoseTurn, gameIsOver,
                     makeMove, getBoard, getPlayers, status, positionsAvailable)

import Maybe

import Board exposing(Mark(..), Board, Position, Move)
import Player exposing(Player(..), extractMark)

type alias Game = {board: Board, players: (Player, Player)}
type Status = Ongoing | Tie | Win Mark

create : (Player, Player) -> Board -> Game
create players board =
    Game board players

whoseTurn : Game -> Maybe Player
whoseTurn ({players} as game) =
    if (gameIsOver (status game)) then
        Nothing
    else
        (Just (Tuple.first players))

gameIsOver : Status -> Bool
gameIsOver status =
    status /= Ongoing

makeMove : Position -> Game -> Game
makeMove position game =
    if (isMoveAllowed position game) then
        addMoveToGame position game
    else
        game

isMoveAllowed : Position -> Game -> Bool
isMoveAllowed position ({board} as game) =
    Board.isPositionAvailable position board
    &&
    status game == Ongoing

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

getBoard : Game -> Board
getBoard {board} =
    board

getPlayers : Game -> (Player, Player)
getPlayers {players} =
    players

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

positionsAvailable : Game -> List Position
positionsAvailable ({board} as game) =
    if (gameIsOver (status game)) then
        []
    else
        Board.positionsAvailable board
