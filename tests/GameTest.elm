module GameTest exposing (gameTests)

import Test exposing (Test, describe, test)
import Expect
import Array

import Helpers
import Game exposing (Status(..))
import Player exposing (Player(..))
import Board exposing (Size(..), Mark(..))

gameTests : Test
gameTests =
    describe "A Game should"
        [ test "know whose turn it is at the start of a game" <|
              \() ->
                  Helpers.createNewGame Standard (Human X, Human O)
                      |> Game.whoseTurn
                      |> Expect.equal (Just (Human X))

        , test "update the current player after a move is made (X starts)" <|
              \() ->
                  Helpers.createNewGame Standard (Human X, Human O)
                      |> Game.makeMove 1
                      |> Game.whoseTurn
                      |> Expect.equal (Just (Human O))

        , test "update the current player after a move is made (O starts)" <|
              \() ->
                  Helpers.createNewGame Standard (Human O, Human X)
                      |> Game.makeMove 4
                      |> Game.whoseTurn
                      |> Expect.equal (Just (Human X))

        , test "not update the current player if a move has already been made" <|
              \() ->
                  Helpers.createNewGame Standard (Human O, Human X)
                      |> Game.makeMove 4
                      |> Game.makeMove 4
                      |> Game.whoseTurn
                      |> Expect.equal (Just (Human X))

        , test "know that it is not anyone's turn when a game is over" <|
              \() ->
                  Helpers.createTieGame
                      |> Game.whoseTurn
                      |> Expect.equal Nothing

        , test "update the board after a move is made (X starts)" <|
              \() ->
                  Helpers.createNewGame Standard (Human X, Human O)
                      |> Game.makeMove 3
                      |> Game.getBoard
                      |> Board.toArray
                      |> Array.get 3
                      |> Expect.equal (Just (Just X))

        , test "update the board after a move is made (O starts)" <|
              \() ->
                  Helpers.createNewGame Standard (Human O, Human X)
                      |> Game.makeMove 5
                      |> Game.getBoard
                      |> Board.toArray
                      |> Array.get 5
                      |> Expect.equal (Just (Just O))

        , test "not update the board if a move has already been made" <|
              \() ->
                  Helpers.createNewGame Standard (Human X, Human O)
                      |> Game.makeMove 5
                      |> Game.makeMove 5
                      |> Game.getBoard
                      |> Board.toArray
                      |> Array.get 5
                      |> Expect.equal (Just (Just X))

        , test "know that a game is ongoing at the start" <|
              \() ->
                  Helpers.createNewGame Standard (Human X, Human O)
                      |> Game.status
                      |> Expect.equal Ongoing

        , test "know when a game is tied" <|
              \() ->
                  Helpers.createTieGame
                      |> Game.status
                      |> Expect.equal Tie

        , test "know when X has won" <|
              \() ->
                  Helpers.createXwinGame
                      |> Game.status
                      |> Expect.equal (Win X)

        , test "know when O has won" <|
              \() ->
                  Helpers.createOwinGame
                      |> Game.status
                      |> Expect.equal (Win O)

        , test "not update the game state if a move is made when the game is over" <|
              \() ->
                  Helpers.createOwinGame
                      |> Game.makeMove 7
                      |> Expect.equal Helpers.createOwinGame

        , test "provide all positions available (all cells at the beginning)" <|
              \() ->
                  Helpers.createNewGame Standard (Human O, Human X)
                      |> Game.positionsAvailable
                      |> Expect.equal (List.range 0 8)

        , test "provide all positions available after a move is made" <|
              \() ->
                  Helpers.createNewGame Standard (Human O, Human X)
                      |> Game.makeMove 0
                      |> Game.positionsAvailable
                      |> Expect.equal (List.range 1 8)

        , test "provide no positions available when the game is over (board still has empty cells)" <|
              \() ->
                  Helpers.createOwinGame
                      |> Game.positionsAvailable
                      |> Expect.equal []
        ]
