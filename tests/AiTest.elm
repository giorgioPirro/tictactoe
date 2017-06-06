module AiTest exposing (aiTests)

import Test exposing (Test, describe, test)
import Expect

import Ai exposing (pickBestPosition)
import Board exposing (Mark(..), Size(..))
import Player exposing (Player(..))
import Helpers exposing (createNewGame, createGameXCanWin, createGameOCanAvoidLoss,
                         createGameXCanBlockFork, createGameOCanMakeFork)

aiTests : Test
aiTests =
    describe "The Ai should"
        [ test "pick the winning move" <|
              \() ->
                  createGameXCanWin
                      |> Ai.pickBestPosition
                      |> Expect.equal (Just 0)

        , test "pick the blocking move" <|
              \() ->
                  createGameOCanAvoidLoss
                      |> Ai.pickBestPosition
                      |> Expect.equal (Just 6)

        , test "stop an inside fork" <|
              \() ->
                  createGameXCanBlockFork
                      |> Ai.pickBestPosition
                      |> Expect.equal (Just 0)

        , test "make a fork" <|
              \() ->
                  createGameOCanMakeFork
                      |> Ai.pickBestPosition
                      |> Expect.equal (Just 2)
        ]
