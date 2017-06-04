module Helpers exposing (createNewGame, createTieGameStandardSizedBoard, createGameXCanWin,
                         createXwinGameStandardSizedBoard, createOwinGameStandardSizedBoard,
                         createDrawBoardStandardSized, standardBoardXwinsHorizontally,
                         standardBoardOwinsHorizontally, standardBoardXwinsVertically,
                         standardBoardOwinsVertically, standardBoardXwinsDownDiagonal,
                         standardBoardOwinsUpDiagonal, randomGameStatus,
                         randomGameOverStatus, randomGameType, randomBoardSize,
                         createGameOCanAvoidLoss, createGameXCanBlockFork,
                         createGameOCanBlockFork)

import Random.Pcg
import Fuzz
import Shrink

import Board exposing (Size(..), Mark(..), Board)
import Game exposing (Game, Player(..), Status(..))
import GameGenerator exposing (GameType(..))

createNewGame : Size -> (Player, Player) -> Game
createNewGame size players =
    Game.create players (Board.create size)

addMovesToBoard : List (Int, Mark) -> Board -> Board
addMovesToBoard moves board =
        List.foldl Board.markCell board moves

createGameXCanWin : Game
createGameXCanWin =
    let
        newGame = Game.create (Human X, Human O) (Board.create Standard)
        moves = [0, 3, 1, 4]
    in
        List.foldl Game.makeMove newGame moves

createGameOCanAvoidLoss : Game
createGameOCanAvoidLoss =
    let
        newGame = Game.create (Human X, Human O) (Board.create Standard)
        moves = [0, 3, 1]
    in
        List.foldl Game.makeMove newGame moves

createGameXCanBlockFork : Game
createGameXCanBlockFork =
    let
        newGame = Game.create (Human O, Human X) (Board.create Standard)
        moves = [4, 2, 6]
    in
        List.foldl Game.makeMove newGame moves

createGameOCanBlockFork : Game
createGameOCanBlockFork =
    let
        newGame = Game.create (Human X, Human O) (Board.create Standard)
        moves = [8, 4, 0]
    in
        List.foldl Game.makeMove newGame moves

createTieGameStandardSizedBoard : Game
createTieGameStandardSizedBoard =
    let
        newGame = Game.create (Human X, Human O) (Board.create Standard)
        moves = [1, 0, 4, 2, 5, 3, 6, 7, 8]
    in
        List.foldl Game.makeMove newGame moves

createXwinGameStandardSizedBoard : Game
createXwinGameStandardSizedBoard =
    let
        newGame = Game.create (Human X, Human O) (Board.create Standard)
        moves = [0, 3, 1, 4, 2]
    in
        List.foldl Game.makeMove newGame moves

createOwinGameStandardSizedBoard : Game
createOwinGameStandardSizedBoard =
    let
        newGame = Game.create (Human O, Human X) (Board.create Standard)
        moves = [0, 3, 4, 5, 8]
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

randomGameStatus : Fuzz.Fuzzer Status
randomGameStatus =
    Fuzz.custom (randomStatusGenerator 0 3) Shrink.noShrink

randomGameOverStatus : Fuzz.Fuzzer Status
randomGameOverStatus =
    Fuzz.custom (randomStatusGenerator 1 3) Shrink.noShrink

randomGameType : Fuzz.Fuzzer GameType
randomGameType =
    Fuzz.custom (randomGameTypeGenerator) Shrink.noShrink

randomBoardSize : Fuzz.Fuzzer Size
randomBoardSize =
    Fuzz.custom (randomBoardSizeGenerator) Shrink.noShrink

intToStatus : Int -> Status
intToStatus statusId =
    case statusId of
        0 -> Ongoing
        1 -> Tie
        2 -> Win X
        _ -> Win O

intToGameType : Int -> GameType
intToGameType statusId =
    case statusId of
        0 -> HumanVsHuman
        1 -> HumanVsComputer
        _ -> ComputerVsComputer

intToBoardSize : Int -> Size
intToBoardSize statusId =
    case statusId of
        0 -> Standard
        _ -> Large

randomStatusGenerator : Int -> Int -> Random.Pcg.Generator Status
randomStatusGenerator startRange endRange =
   Random.Pcg.map intToStatus (Random.Pcg.int startRange endRange)

randomGameTypeGenerator : Random.Pcg.Generator GameType
randomGameTypeGenerator =
   Random.Pcg.map intToGameType (Random.Pcg.int 0 2)

randomBoardSizeGenerator : Random.Pcg.Generator Size
randomBoardSizeGenerator =
  Random.Pcg.map intToBoardSize (Random.Pcg.int 0 1)
