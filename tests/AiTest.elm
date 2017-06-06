module AiTest exposing (aiTests)

import Test exposing (Test, describe, test)
import Expect

import Ai exposing (pickBestPosition)
import Board exposing (Mark(..), Size(..))
import Player exposing (Player(..))
import Helpers

aiTests : Test
aiTests =
    describe "The Ai should"
        [ test "pick the winning move" <|
              \() ->
                  Helpers.createGameXCanWin
                      |> Ai.pickBestPosition
                      |> Expect.equal (Just 0)

        , test "pick the blocking move" <|
              \() ->
                  Helpers.createGameOCanAvoidLoss
                      |> Ai.pickBestPosition
                      |> Expect.equal (Just 6)

        , test "stop an inside fork" <|
              \() ->
                  Helpers.createGameXCanBlockFork
                      |> Ai.pickBestPosition
                      |> Expect.equal (Just 0)

        , test "make a fork" <|
              \() ->
                  Helpers.createGameOCanMakeFork
                      |> Ai.pickBestPosition
                      |> Expect.equal (Just 2)

        , test "block an outside fork" <|
              \() ->
                  Helpers.createGameXCanBlockOutsideFork
                      |> Ai.pickBestPosition
                      |> Expect.equal (Just 1)
        ]
