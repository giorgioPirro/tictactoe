module GameTest exposing (gameTests)

import Test exposing (Test, describe, test, todo)
import Expect
import Array

import Helpers exposing (createNewGame)
import Game
import Board exposing(Size(..), Mark(..))

gameTests : Test
gameTests =
    describe "A Game should"
        [ test "know whose turn it is at the start of a game" <|
              \() ->
                  createNewGame Standard X
                      |> Game.whoseTurn
                      |> Expect.equal X

        , test "update the current player after a move is made (X starts)" <|
              \() ->
                  createNewGame Standard X
                      |> Game.makeMove 1
                      |> Game.whoseTurn
                      |> Expect.equal O

        , test "update the current player after a move is made (O starts)" <|
              \() ->
                  createNewGame Standard O
                      |> Game.makeMove 1
                      |> Game.whoseTurn
                      |> Expect.equal X

        , test "update the board after a move is made (X starts)" <|
              \() ->
                  createNewGame Standard X
                      |> Game.makeMove 3
                      |> Game.getBoard
                      |> Board.toArray
                      |> Array.get 3
                      |> Expect.equal (Just (Just X))

        , test "update the board after a move is made (O starts)" <|
              \() ->
                  createNewGame Standard O
                      |> Game.makeMove 5
                      |> Game.getBoard
                      |> Board.toArray
                      |> Array.get 5
                      |> Expect.equal (Just (Just O))
        ]
