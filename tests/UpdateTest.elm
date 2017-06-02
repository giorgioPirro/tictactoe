module UpdateTest exposing (updateTests)

import Test exposing (Test, describe, test)
import Expect
import Task exposing (Task)

import Helpers exposing (createNewGame)

import Main exposing (Msg(..), Model, update)
import Board exposing (Mark(..), Size(..))
import Game exposing (Player(..))

updateTests : Test
updateTests =
    describe "Update"
        [ describe "given the Move message should"
            [ test "have a 'none' command when the next player to make a move is of Human nature" <|
                \() ->
                    update (MakeMove 0) {game = (createNewGame Standard ((Human X), (Human O)))}
                        |> Tuple.second
                        |> Expect.equal Cmd.none

            , Test.todo "have a move command when the next player to make a move is of Computer nature"
            ]
        ]

