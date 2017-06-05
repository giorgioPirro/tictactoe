module AiTest exposing (aiTests)

import Test exposing (Test, describe, test)
import Expect

import Ai exposing (pickBestPosition)
import Board exposing (Mark(..), Size(..))
import Player exposing (Player(..))
import Helpers exposing (createNewGame, createGameXCanWin, createGameOCanAvoidLoss,
                         createGameXCanBlockFork, createGameOCanBlockFork)

aiTests : Test
aiTests =
    describe "The Ai should"
        [ test "pick the first move available when there are more than 8 to choose from" <|
              \() ->
                  createNewGame Standard (Computer X, Human O)
                      |> Ai.pickBestPosition
                      |> Expect.equal (Just 0)

        , test "pick the winning move" <|
              \() ->
                  createGameXCanWin
                      |> Ai.pickBestPosition
                      |> Expect.equal (Just 2)

        , test "pick the blocking move" <|
              \() ->
                  createGameOCanAvoidLoss
                      |> Ai.pickBestPosition
                      |> Expect.equal (Just 2)

        , test "stop an inside fork" <|
              \() ->
                  createGameXCanBlockFork
                      |> Ai.pickBestPosition
                      |> Expect.equal (Just 0)

        , test "stop an outside fork" <|
              \() ->
                  createGameOCanBlockFork
                      |> Ai.pickBestPosition
                      |> Expect.equal (Just 1)
        ]
