module GameTest exposing (gameTests)

import Test exposing (Test, describe, test, todo)
import Expect
import Array

import Helpers exposing (createNewGame, createTieGameStandardSizedBoard,
                         createXwinGameStandardSizedBoard, createOwinGameStandardSizedBoard)
import Game exposing (Player(..), Status(..))
import Board exposing (Size(..), Mark(..))

gameTests : Test
gameTests =
    describe "A Game should"
        [ test "know whose turn it is at the start of a game" <|
              \() ->
                  createNewGame Standard (Human X, Human O)
                      |> Game.whoseTurn
                      |> Expect.equal (Just (Human X))

        , test "update the current player after a move is made (X starts)" <|
              \() ->
                  createNewGame Standard (Human X, Human O)
                      |> Game.makeMove 1
                      |> Game.whoseTurn
                      |> Expect.equal (Just (Human O))

        , test "know whose turn it is next (X starts)" <|
              \() ->
                  createNewGame Standard (Human X, Human O)
                      |> Game.makeMove 1
                      |> Game.whoseTurnNext
                      |> Expect.equal (Just (Human X))

        , test "know whose turn it is next (O starts)" <|
              \() ->
                  createNewGame Standard (Human O, Human X)
                      |> Game.makeMove 1
                      |> Game.whoseTurnNext
                      |> Expect.equal (Just (Human O))

        , test "know that it is not anyone's turn when a game is over" <|
              \() ->
                  createTieGameStandardSizedBoard
                      |> Game.whoseTurn
                      |> Expect.equal Nothing

        , test "know that it is not anyone's turn next when a game is over" <|
              \() ->
                  createTieGameStandardSizedBoard
                      |> Game.whoseTurnNext
                      |> Expect.equal Nothing

        , test "update the current player after a move is made (O starts)" <|
              \() ->
                  createNewGame Standard (Human O, Human X)
                      |> Game.makeMove 4
                      |> Game.whoseTurn
                      |> Expect.equal (Just (Human X))

        , test "not update the current player if a move has alread been made (O starts)" <|
              \() ->
                  createNewGame Standard (Human O, Human X)
                      |> Game.makeMove 4
                      |> Game.makeMove 4
                      |> Game.whoseTurn
                      |> Expect.equal (Just (Human X))

        , test "update the board after a move is made (X starts)" <|
              \() ->
                  createNewGame Standard (Human X, Human O)
                      |> Game.makeMove 3
                      |> Game.getBoard
                      |> Board.toArray
                      |> Array.get 3
                      |> Expect.equal (Just (Just X))

        , test "update the board after a move is made (O starts)" <|
              \() ->
                  createNewGame Standard (Human O, Human X)
                      |> Game.makeMove 5
                      |> Game.getBoard
                      |> Board.toArray
                      |> Array.get 5
                      |> Expect.equal (Just (Just O))

        , test "not update the board if a move has already been made (X starts)" <|
              \() ->
                  createNewGame Standard (Human X, Human O)
                      |> Game.makeMove 5
                      |> Game.makeMove 5
                      |> Game.getBoard
                      |> Board.toArray
                      |> Array.get 5
                      |> Expect.equal (Just (Just X))

        , test "know that a game is ongoing at the start" <|
              \() ->
                  createNewGame Standard (Human X, Human O)
                      |> Game.status
                      |> Expect.equal Ongoing

        , test "know when a game is tied" <|
              \() ->
                  createTieGameStandardSizedBoard
                      |> Game.status
                      |> Expect.equal Tie

        , test "know when X has won" <|
              \() ->
                  createXwinGameStandardSizedBoard
                      |> Game.status
                      |> Expect.equal (Win X)

        , test "know when O has won" <|
              \() ->
                  createOwinGameStandardSizedBoard
                      |> Game.status
                      |> Expect.equal (Win O)

        , test "not update the game state if a move is made when the game is over" <|
              \() ->
                  createOwinGameStandardSizedBoard
                      |> Game.makeMove 7
                      |> Expect.equal createOwinGameStandardSizedBoard
        ]
