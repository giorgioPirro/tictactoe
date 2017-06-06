module Helpers exposing (..)

import Random.Pcg
import Fuzz
import Shrink

import Board exposing (Size(..), Mark(..), Board)
import Game exposing (Game, Status(..))
import Player exposing (Player(..))
import GameGenerator exposing (GameType(..))

createNewGame : Size -> (Player, Player) -> Game
createNewGame size players =
    Game.create players (Board.create size)

parseStandardSizedGame : (Player, Player) -> List String -> Game
parseStandardSizedGame players cellsToParse =
         Game.create players (parseStandardSizeBoard cellsToParse)

parseStandardSizeBoard : List String -> Board
parseStandardSizeBoard cellsToParse =
    let
        movesToMake = cellsToParse
            |> List.indexedMap (,)
            |> List.filter (\(position,cell) -> cell /= "-")
            |> List.map (\(position, cell) -> case cell of
                                                  "X" -> (position, X)
                                                  _   -> (position, O))
    in
        addMovesToBoard movesToMake (Board.create Standard)

addMovesToBoard : List (Int, Mark) -> Board -> Board
addMovesToBoard moves board =
        List.foldl Board.markCell board moves

createGameXCanWin : Game
createGameXCanWin =
    parseStandardSizedGame (Computer X, Human O) [ "-", "-", "O"
                                                 , "X", "X", "O"
                                                 , "O", "-", "X"
                                                 ]

createGameOCanAvoidLoss : Game
createGameOCanAvoidLoss =
    parseStandardSizedGame (Computer O, Human X) [ "O", "O", "X"
                                                 , "-", "X", "-"
                                                 , "-", "-", "-"
                                                 ]

createGameXCanBlockFork : Game
createGameXCanBlockFork =
    parseStandardSizedGame (Computer X, Human O) [ "-", "-", "X"
                                                 , "-", "O", "-"
                                                 , "O", "-", "-"
                                                 ]

createGameOCanMakeFork : Game
createGameOCanMakeFork =
    parseStandardSizedGame (Computer O, Human X) [ "X", "-", "-"
                                                 , "-", "O", "-"
                                                 , "-", "-", "O"
                                                 ]

createGameXCanBlockOutsideFork : Game
createGameXCanBlockOutsideFork =
    parseStandardSizedGame (Computer X, Human O) [ "O", "-", "-"
                                                 , "-", "X", "-"
                                                 , "-", "-", "O"
                                                 ]

createTieGame : Game
createTieGame =
    parseStandardSizedGame (Human X, Human O) [ "O", "X", "O"
                                              , "O", "X", "X"
                                              , "X", "O", "X"
                                              ]

createXwinGame : Game
createXwinGame =
    parseStandardSizedGame (Human X, Human O) [ "X", "X", "X"
                                              , "O", "O", "-"
                                              , "-", "-", "-"
                                              ]

createOwinGame : Game
createOwinGame =
    parseStandardSizedGame (Human O, Human X) [ "O", "-", "-"
                                              , "X", "O", "X"
                                              , "-", "-", "O"
                                              ]



createDrawBoardStandardSized : Board
createDrawBoardStandardSized =
    parseStandardSizeBoard [ "O", "X", "O"
                           , "O", "X", "X"
                           , "X", "O", "X"
                           ]

standardBoardXwinsHorizontally : Board
standardBoardXwinsHorizontally =
    parseStandardSizeBoard [ "X", "X", "X"
                           , "O", "O", "-"
                           , "-", "-", "-"
                           ]

standardBoardOwinsHorizontally : Board
standardBoardOwinsHorizontally =
    parseStandardSizeBoard [ "X", "X", "-"
                           , "-", "-", "-"
                           , "O", "O", "O"
                           ]

standardBoardXwinsVertically : Board
standardBoardXwinsVertically =
    parseStandardSizeBoard [ "X", "O", "O"
                           , "X", "-", "-"
                           , "X", "-", "-"
                           ]

standardBoardOwinsVertically : Board
standardBoardOwinsVertically =
    parseStandardSizeBoard [ "X", "O", "-"
                           , "X", "O", "-"
                           , "-", "O", "-"
                           ]

standardBoardXwinsDownDiagonal : Board
standardBoardXwinsDownDiagonal =
    parseStandardSizeBoard [ "X", "O", "-"
                           , "-", "X", "O"
                           , "-", "-", "X"
                           ]

standardBoardOwinsUpDiagonal : Board
standardBoardOwinsUpDiagonal =
    parseStandardSizeBoard [ "X", "-", "O"
                           , "-", "O", "X"
                           , "O", "-", "-"
                           ]



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
