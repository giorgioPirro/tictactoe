module UpdateTest exposing (updateTests)

import Test exposing (Test, describe, test, fuzz)
import Expect
import Task exposing (Task)
import Array

import Helpers exposing (createNewGame, randomBoardSize, randomGameType)
import Msg exposing (Msg(..))
import Main exposing (Model, update)
import Board exposing (Mark(..), Size(..))
import Game
import Player exposing (Player(..))
import GameGenerator exposing (GameType(..))

updateTests : Test
updateTests =
    describe "Update"
        [ describe "given the Move message should"
            [ test "have a 'none' command when the next player to make a move is of Human nature" <|
                \() ->
                    update (MakeMove 0) {game = (createNewGame Standard ((Human X), (Human O)))}
                        |> Tuple.second
                        |> Expect.equal Cmd.none

            , test "update the board with the given move" <|
                \() ->
                    update (MakeMove 0) {game = (createNewGame Standard ((Human X), (Human O)))}
                        |> Tuple.first
                        |> .game
                        |> Game.getBoard
                        |> Board.toArray
                        |> Array.get 0
                        |> Expect.equal (Just (Just X))
            ]

        , describe "given a NewGame message should"
            [ fuzz randomBoardSize "set the game to the size requested" <|
                  \aBoardSize ->
                      update (NewGame aBoardSize HumanVsHuman) {game = (createNewGame Standard ((Human X), (Human O)))}
                          |> Tuple.first
                          |> .game
                          |> Game.getBoard
                          |> Board.size
                          |> Expect.equal aBoardSize

            , fuzz randomGameType "set the game to the type requested" <|
                  \aGameType ->
                      update (NewGame Standard aGameType) {game = (createNewGame Standard ((Human X), (Human O)))}
                          |> Tuple.first
                          |> .game
                          |> Game.getPlayers
                          |> GameGenerator.whichGameType
                          |> Expect.equal aGameType
            ]
        ]

