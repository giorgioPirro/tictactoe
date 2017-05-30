module GameTest exposing (gameTests)

import Test exposing (Test, describe, test, todo)
import Expect
import Array

import Helpers exposing (createNewGame)
import Game exposing (Player(..))
import Board exposing (Size(..), Mark(..))

gameTests : Test
gameTests =
    describe "A Game should"
        [ test "know whose turn it is at the start of a game" <|
              \() ->
                  Board.create Standard
                      |> Game.newCreate (Human X, Human O)
                      |> Game.newWhoseTurn
                      |> Expect.equal (Just (Human X))

        , test "update the current player after a move is made (X starts)" <|
              \() ->
                  Board.create Standard
                      |> Game.newCreate (Human X, Human O)
                      |> Game.makeMove 1
                      |> Game.newWhoseTurn
                      |> Expect.equal (Just (Human O))

        , todo "test whoseturn should return nothing when the game is over"

        , test "update the current player after a move is made (O starts)" <|
              \() ->
                  Board.create Standard
                      |> Game.newCreate (Human O, Human X)
                      |> Game.makeMove 4
                      |> Game.newWhoseTurn
                      |> Expect.equal (Just (Human X))

        , test "update the board after a move is made (X starts)" <|
              \() ->
                  Board.create Standard
                      |> Game.newCreate (Human X, Human O)
                      |> Game.makeMove 3
                      |> Game.getBoard
                      |> Board.toArray
                      |> Array.get 3
                      |> Expect.equal (Just (Just X))

        , test "update the board after a move is made (O starts)" <|
              \() ->
                  Board.create Standard
                      |> Game.newCreate (Human O, Human X)
                      |> Game.makeMove 5
                      |> Game.getBoard
                      |> Board.toArray
                      |> Array.get 5
                      |> Expect.equal (Just (Just O))
        ]
